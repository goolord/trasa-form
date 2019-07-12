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
formGen :: (Applicative m)
  => Url -- ^ action url
  -> [(Text,Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGen action hidden children = do 
  form_ [action_ (encodeUrl action), method_ "POST", enctype_ "multipart/form-data"] $
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
  -> Form m input error (HtmlT f ()) () ()
label c = G.label mkLabel
  where
  mkLabel i = label_ [for_ $ tshow i] c

formFoo :: SimpleForm Foo
formFoo = Foo 
  <$> label "Text Field 1" ++> inputText Right ""
  <*> label "Text Field 2" ++> inputText Right ""
  <*> label "Int Field 1" ++> inputInt readInt 0

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

simpleReform :: (Monad m, Alternative m, Applicative f) => Url -> Form (TrasaT m) (Rec Identity caps) error (HtmlT f ()) proof b -> TrasaT m (HtmlT f ())
simpleReform action = reform (formGen action) "reform-prefix" (const (pure ())) Nothing

reform :: (MonadPlus m, Alternative m, Monoid view)  
  => ([(Text, Text)] -> view -> view)  -- wrap raw form html inside a <form> tag
  -> Text -- Prefix
  -> (a -> m b) -- success handler used when form validates
  -> Maybe ([(FormRange, error)] -> view -> m b)  -- failure handler used when form does not validate
  -> Form m (Rec Identity caps) error view proof a  -- the formlet
  -> m view
reform toForm prefix handleSuccess handleFailure formlet = do 
  tguard prefix (reformSingle toForm' prefix handleSuccess handleFailure formlet)
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view
  tguard :: (MonadPlus m) => Text -> m a -> m a
  tguard formName part =
    part `mplus` part

reformSingle :: (Monad m, Monoid view)
  => ([(Text, Text)] -> view -> view)
  -> Text
  -> (a -> m b)
  -> Maybe ([(FormRange, error)] -> view -> m b)
  -> Form m (Rec Identity caps) error view proof a
  -> m view
reformSingle toForm prefix handleSuccess mhandleFailure formlet = do
  (View viewf, res') <- runForm NoEnvironment (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error err -> undefined
    Ok (Proved proofs pos unProved) -> do
      handleSuccess unProved
      pure $ toForm [] $ viewf []
