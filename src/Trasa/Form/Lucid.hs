{-# language OverloadedStrings #-}

module Trasa.Form.Lucid where

import Control.Monad.Except
import Data.Text (Text)
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Types
import Lucid
import Trasa.Form
import Trasa.Server
import Trasa.Url

formGET :: (MonadIO m, Applicative f) 
  => Text
  -> Form (TrasaT m) QueryParam err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
formGET action = reform (formGenGET action) "ditto"

formPOST :: (MonadIO m, Applicative f) 
  => Text
  -> FormData b
  -> Form (TrasaT m) QueryParam err (HtmlT f ()) b 
  -> TrasaT m (Result err b, HtmlT f ())
formPOST action formData = reformPost (formGenPOST action) "ditto" formData
