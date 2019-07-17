{-# language OverloadedStrings #-}

module Trasa.Form.Lucid where

import Trasa.Form
import Control.Monad.Except
import Data.Text (Text)
import Lucid
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Result
import Trasa.Server
import Trasa.Url

queryParamReformGET :: (MonadIO m, Show b, Applicative f) 
  => Text
  -> Form (TrasaT m) QueryParam err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
queryParamReformGET action = reformQP (formGenGET' action) "reform"

queryParamReformPOST :: (MonadIO m, Show b, Applicative f) 
  => Text
  -> Form (TrasaT m) QueryParam err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
queryParamReformPOST action = reformQP (formGenPOST' action) "reform"

simpleReformGET :: (MonadIO m, Show b, Applicative f) 
  => Text
  -> Form (TrasaT m) Text err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
simpleReformGET action = reform (formGenGET' action) "reform"

simpleReformPOST :: (MonadIO m, Show b, Applicative f) 
  => Text
  -> Form (TrasaT m) Text err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
simpleReformPOST action = reform (formGenPOST' action) "reform"

formGenGET' :: Applicative f => Text -> [(Text, Text)] -> HtmlT f b -> HtmlT f b
formGenGET' url = formGenGET url

formGenPOST' :: Applicative f => Text -> [(Text, Text)] -> HtmlT f b -> HtmlT f b
formGenPOST' url = formGenPOST url
