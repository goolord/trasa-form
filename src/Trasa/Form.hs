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

-- {-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Trasa.Form where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.CaseInsensitive
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Text (Text)
import Lucid
import System.Random
import Text.Reform.Backend
import Text.Reform.Core hiding (view)
import Text.Reform.Result
-- import Topaz.Rec ((<:))
import Trasa.Core hiding (optional)
import Trasa.Server
import Trasa.Url
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import qualified Network.HTTP.Types as HTTP
import qualified Text.Read
import qualified Text.Reform.Generalized as G
import qualified Trasa.Method as M
import qualified Web.Cookie as Cookie

---- REFORM ----

tshow :: Show a => a -> Text
tshow = T.pack . show

inputText :: (Monad m, FormError err, Applicative f) 
  => (input -> Either err Text)
  -> Text
  -> Form m input err (HtmlT f ()) () Text
inputText getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "text", id_ (tshow i), name_ (tshow i), value_ a]

inputInt :: (Monad m, FormError err, Applicative f)
  => (input -> Either err Int)
  -> Int
  -> Form m input err (HtmlT f ()) () Int
inputInt getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "number", id_ (tshow i), name_ (tshow i), value_ (tshow a)]

buttonSubmit :: (Monad m, FormError err, Monad f, ToHtml children) 
  => (input -> Either err Text)
  -> Text
  -> children
  -> Form m input err (HtmlT f ()) () (Maybe Text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField text
  where
  inputField i a = button_ [type_ "submit", id_ (tshow i), name_ (tshow i), value_ (tshow a)] $ toHtml c

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
formGen :: (Applicative m)
  => Url -- ^ action url
  -> [(Text,Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGen action hidden children = do 
  form_ [action_ (encodeUrl action), method_ "GET", enctype_ "multipart/form-data"] $
       traverse_ mkHidden hidden
    *> children
  where
  mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

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
  Right (i, _) -> Right i

label :: (Monad m, Monad f) =>
     HtmlT f ()
  -> Form m input err (HtmlT f ()) () ()
label c = G.label mkLabel
  where
  mkLabel i = label_ [for_ $ tshow i] c

formFoo :: SimpleForm Foo
formFoo = Foo 
  <$> label "Text Field 1" ++> inputText Right ""
  <*> label "Text Field 2" ++> inputText Right ""
  <*> label "Int Field 1" ++> inputInt readInt 0

getHeader :: CI BS.ByteString -> TrasaT IO (Maybe T.Text)
getHeader idt = fmap (M.lookup idt . trasaHeaders) ask

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

simpleReform :: (MonadIO m, Show b, Applicative f) => Url -> Form (TrasaT m) Text err (HtmlT f ()) proof b -> TrasaT m (HtmlT f ())
simpleReform action = reform (formGen action) "reform" (\x -> liftIO $ print x) Nothing

reform :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view)  -- wrap raw form html inside a <form> tag
  -> Text -- Prefix
  -> (a -> TrasaT m b) -- success handler used when form validates
  -> Maybe ([(FormRange, err)] -> view -> TrasaT m b)  -- failure handler used when form does not validate
  -> Form (TrasaT m) Text err view proof a  -- the formlet
  -> TrasaT m view
reform toForm prefix handleSuccess handleFailure formlet = do 
  tguard prefix (reformSingle toForm' prefix handleSuccess handleFailure formlet)
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view
  tguard :: (Monad m) => Text -> m a -> m a
  tguard formName part = part

reformSingle :: (MonadIO m, Monoid view)
  => ([(Text, Text)] -> view -> view)
  -> Text
  -> (a -> TrasaT m b)
  -> Maybe ([(FormRange, err)] -> view -> TrasaT m b)
  -> Form (TrasaT m) Text err view proof a
  -> TrasaT m view
reformSingle toForm prefix handleSuccess mHandleFailure formlet = do
  (View viewf, res') <- runForm (Environment env) (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error errs -> case mHandleFailure of
      Just handleFailure -> do
        handleFailure errs (toForm [] (viewf errs))
        pure $ toForm [] $ viewf errs
      Nothing -> pure $ toForm [] $ viewf errs
    Ok (Proved proofs pos unProved) -> do
      handleSuccess unProved
      pure $ toForm [] $ viewf []
  where
  env :: MonadIO m => FormId -> TrasaT m (Value Text)
  env formId = do
    QueryString queryString <- getQueryString
    let val = HM.lookup (tshow formId) queryString
    case val of
      Nothing -> pure Missing
      Just QueryParamFlag -> pure Default
      Just (QueryParamSingle x) -> pure (Found x)
      Just (QueryParamList x) -> pure (Found $ T.intercalate ", " x) -- ???

getQueryString :: MonadIO m => TrasaT m QueryString
getQueryString = trasaQueryString <$> ask
