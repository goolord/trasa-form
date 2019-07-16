{-# language OverloadedStrings #-}

module Trasa.Form.Lucid where

import Trasa.Form
import Control.Monad.Except
import Data.Text (Text)
import Lucid
import Text.Reform.Core hiding (view)
import Text.Reform.Lucid.Common
import Text.Reform.Result
import Trasa.Core hiding (optional)
import Trasa.Server

simpleReformGET :: (MonadIO m, Show b, Applicative f) 
  => Url 
  -> Form (TrasaT m) Text err (HtmlT f ()) proof b 
  -> TrasaT m (Result err b, HtmlT f ())
simpleReformGET action = reform (formGenGET' action) "reform"

simpleReformPOST :: (MonadIO m, Show b, Applicative f) 
  => Url 
  -> Form (TrasaT m) Text err (HtmlT f ()) proof b 
  -> TrasaT m (Result err b, HtmlT f ())
simpleReformPOST action = reform (formGenPOST' action) "reform"

formGenGET' :: Applicative f => Url -> [(Text, Text)] -> HtmlT f b -> HtmlT f b
formGenGET' url = formGenGET (encodeUrl url)

formGenPOST' :: Applicative f => Url -> [(Text, Text)] -> HtmlT f b -> HtmlT f b
formGenPOST' url = formGenPOST (encodeUrl url)
