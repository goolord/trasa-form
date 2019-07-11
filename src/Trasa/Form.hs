{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}
{-# language ExplicitForAll #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}

module Trasa.Form where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.CaseInsensitive
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import Data.Time
import GHC.Generics hiding (Meta)
import Lucid
import System.Random
import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Result (FormId, Result(..), unitRange, FormRange(..))
import Topaz.Rec ((<:))
import Trasa.Core hiding (optional)
import Trasa.Server
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import qualified Text.Read
import qualified Text.Reform.Generalized as G
import qualified Trasa.Method as M
import qualified Web.Cookie as Cookie

foo :: Meta capCodec qryCodec reqCodec respCodec caps qrys req resp 
  -> Arguments caps qrys req (Field a)
  -> HtmlT m a
foo Meta{..} args = do
  let pathFs :: Path caps qrys -> [Formlet a]
      pathFs PathNil = []
      pathFs (PathConsCapture _ as) = undefined
      pathFs (PathConsMatch _ as) = undefined
  let queryFs :: Rec (Query qryCodec) qrys -> [Formlet a]
      queryFs RecNil = []
      queryFs (RecCons q rs) = undefined
  let queryF :: Query qryCodec r -> (Text -> a -> Html ()) -> Formlet a
      queryF (QueryFlag t) f = Formlet $ f t
      queryF (QueryOptional t _) f = Formlet $ f t
      queryF (QueryList t _) f = Formlet $ f t
  undefined

newtype Formlet a = Formlet { getFormlet :: a -> Html () }

data Field a = Field ( Text -> a -> Html () )

-------------------------------------------------
data Counter = Red | Green | Blue
  deriving (Show,Read)
data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  AssignR :: Route '[Counter,Int] '[] 'Bodyless ()
  IncrementR :: Route '[Counter] '[] 'Bodyless Int
  QueryR :: Route '[Counter] '[] Bodyless Int
  TotalR :: Route '[] '[] 'Bodyless Int
int :: CaptureCodec Int
int = showReadCaptureCodec
counter :: CaptureCodec Counter
counter = showReadCaptureCodec
bodyUnit :: BodyCodec ()
bodyUnit = BodyCodec (pure "text/plain") (const "") (const (Right ()))
bodyInt :: BodyCodec Int
bodyInt = showReadBodyCodec
meta :: Route captures querys request response -> MetaCodec captures querys request response
meta x = metaBuilderToMetaCodec $ case x of
  AssignR -> Meta
    (match "assign" ./ capture counter ./ match "to" ./ capture int ./ end)
    qend
    bodyless (resp bodyUnit) M.post
  IncrementR -> Meta
    (match "increment" ./ capture counter ./ end)
    qend
    bodyless (resp bodyInt) M.post
  QueryR -> Meta
    (match "query" ./ capture counter ./ end)
    qend
    bodyless (resp bodyInt) M.get
  TotalR -> Meta
    (match "total" ./ end)
    qend
    bodyless (resp bodyInt) M.get

---- REFORM ----

tshow :: Show a => a -> Text
tshow = T.pack . show

inputText :: (Monad m, FormError error, Applicative f) =>
             (input -> Either error Text)
          -> Text
          -> Form m input error (HtmlT f ()) () Text
inputText getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "text", id_ (tshow i), name_ (tshow i), value_ a]

inputInt :: (Monad m, FormError error, Applicative f) =>
             (input -> Either error Int)
          -> Int
          -> Form m input error (HtmlT f ()) () Int
inputInt getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "text", id_ (tshow i), name_ (tshow i), value_ (tshow a)]

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (Applicative m, IsRoute route)
  => Prepared route response -- ^ action url
  -> [(Text,Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
form action hidden children = do 
  form_ [action_ (encodeRoute action), method_ "POST", enctype_ "multipart/form-data"] $
       traverse_ mkHidden hidden
    *> children
  where
  mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

class IsRoute route where
  routeMeta :: Prepared route response -> Meta CaptureEncoding CaptureEncoding reqCodec respCodec caps qrys req resp

link :: IsRoute route => Prepared route response -> Url
link rt = linkWith (const (routeMeta rt)) rt

encodeRoute :: IsRoute route => Prepared route response -> Text
encodeRoute = encodeUrl . link

instance FormError Text where
  type ErrorInputType Text = Text
  commonFormError = T.pack . (commonFormErrorStr T.unpack)

data Foo = Foo Text Text Int
type SimpleForm a = Form Identity Text Text (Html ()) () a

readMay :: Read a => Text -> Maybe a
readMay = Text.Read.readMaybe . T.unpack

readInt :: Text -> Either Text Int
readInt input = case TR.decimal input of
  Left err -> Left $ T.pack err
  Right (int, _) -> Right int

label :: (Monad m, Monad f) =>
     HtmlT f ()
  -> Form m input error (HtmlT f ()) () ()
label c = G.label mkLabel
  where
  mkLabel i = label_ [for_ $ tshow i] c

formFoo :: SimpleForm Foo
formFoo = Foo 
  <$> label "Text Field 1" ++> inputText Right ""
  <*> label "Text Field 2" ++> inputText Right ""
  <*> label "Int Field 1" ++> inputInt readInt 0

-- reform ::
--   ([(Text, Text)] -> view -> view)            -- ^ wrap raw form html inside a @\<form\>@ tag
--   -> Text                                        -- ^ prefix
--   -> (a -> m b)                                  -- ^ success handler used when form validates
--   -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ failure handler used when form does not validate
--   -> Form m [Input] error view proof a           -- ^ the formlet
--   -> m view
reform :: (Monoid view) =>
            ([(Text, Text)] -> view -> view)            -- ^ wrap raw form html inside a @\<form\>@ tag
         -> Text                                        -- ^ prefix
         -> (a -> TrasaT IO b)                                  -- ^ success handler used when form validates
         -> Maybe ([(FormRange, error)] -> view -> TrasaT IO b) -- ^ failure handler used when form does not validate
         -> Form (TrasaT IO) [Text] error view proof a           -- ^ the formlet
         -> TrasaT IO view
reform toForm prefix success failure form =
    guard prefix (reformSingle toForm' (TL.fromStrict prefix) success failure form)
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view
  guard :: (MonadPlus m) => Text -> m a -> m a
  guard formName part = ( do 
    undefined
    -- method POST
    -- submittedName <- getDataFn (lookText "formname")
    -- if (submittedName == (Right formName))
    -- then part
    -- else localRq (\req -> req { rqMethod = GET }) part
    ) `mplus` part

reformSingle :: (MonadPlus m, Alternative m, Monoid view) =>
                  ([(Text, Text)] -> view -> view)            -- ^ wrap raw form html inside a <form> tag
               -> TL.Text                                      -- ^ prefix
               -> (a -> m b)                                  -- ^ handler used when form validates
               -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ handler used when form does not validate
               -> Form (TrasaT IO) [Text] error view proof a           -- ^ the formlet
               -> TrasaT IO view
reformSingle toForm prefix handleSuccess mHandleFailure form =
    msum [ do csrfToken <- addCSRFCookie (T.encodeUtf8 csrfName)
              toForm [(csrfName, csrfToken)] <$> viewForm prefix form

         , do checkCSRF csrfName
              undefined
           --    (v, mresult) <- runForm environment prefix form
           --    result <- mresult
           --    case result of
           --      (Ok a)         ->
           --          (escape . fmap toResponse) $ do -- expireCookie csrfName
           --                                          handleSuccess (unProved a)
           --      (Error errors) ->
           --          do csrfToken <- addCSRFCookie csrfName
           --             case mHandleFailure of
           --               (Just handleFailure) ->
           --                   (escape . fmap toResponse) $
           --                     handleFailure errors (toForm [(csrfName, csrfToken)] (unView v errors))
           --               Nothing ->
           --                   pure $ toForm [(csrfName, csrfToken)] (unView v errors)
         ]

csrfName :: Text
csrfName = "reform-csrf"

-- | Utility Function: add a cookie for CSRF protection
addCSRFCookie :: BS.ByteString    -- ^ name to use for the cookie
  -> TrasaT IO Text
addCSRFCookie name = do
  mc <- lookupCookie name
  case mc of
    Nothing -> do 
      i :: Integer <- liftIO randomIO
      setCookie $ Cookie.defaultSetCookie
        { Cookie.setCookieName = name
        , Cookie.setCookieValue = BC8.pack $ show i
        , Cookie.setCookieHttpOnly = True 
        , Cookie.setCookieExpires = Nothing
        }
      pure $ tshow i
    Just c -> pure $ T.decodeUtf8 c

getHeader :: CI BS.ByteString -> TrasaT IO (Maybe T.Text)
getHeader idt = fmap (M.lookup idt) ask

currentHeader :: CI BS.ByteString -> TrasaT IO (Maybe T.Text)
currentHeader idt = fmap (M.lookup idt) get

setHeader :: CI BS.ByteString -> T.Text -> TrasaT IO ()
setHeader idt header = modify (M.insert idt header)

getCookies :: TrasaT IO (Maybe Cookie.Cookies)
getCookies = getHeader "Cookie" >>= \case
  Nothing -> pure Nothing
  Just rawCookie -> pure $ Just $ Cookie.parseCookies $ T.encodeUtf8 rawCookie

lookupCookie :: BS.ByteString -> TrasaT IO (Maybe BS.ByteString)
lookupCookie name = do
  getCookies >>= \case 
    Nothing -> pure Nothing
    Just cookies -> pure $ lookup name cookies

setCookie :: Cookie.SetCookie -> TrasaT IO ()
setCookie cookie = do
  let cookie' :: Text
      cookie' = T.decodeUtf8 $ BL.toStrict $ BB.toLazyByteString $ Cookie.renderSetCookie cookie
  setHeader "Set-Cookie" cookie'

checkCSRF :: Text -> TrasaT IO ()
checkCSRF name = do
  mc <- getCSRFCookie name
  mi <- lookText (TL.unpack name)
  case (mc, mi) of
    (Just c, Just c')
        | c == c' -> pure ()
    _ -> throwError (TrasaErr HTTP.status403 "CSRF check failed.")
