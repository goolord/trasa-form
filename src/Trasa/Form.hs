{-# language ConstraintKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trasa.Form 
  ( reform
  , SimpleForm
  , FormError(..)
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Text (Text)
import Lucid
import Ditto.Backend
import Ditto.Core hiding (view)
import Ditto.Result
import Trasa.Core hiding (optional)
import Trasa.Server
import Trasa.Url
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

---- REFORM ----

tshow :: Show a => a -> Text
tshow = T.pack . show

instance FormError Text where
  type ErrorInputType Text = Text
  commonFormError = T.pack . (commonFormErrorStr T.unpack)

type SimpleForm a = Form Identity Text Text (Html ()) a

reform :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form (TrasaT m) Text err view a  -- ^ the formlet
  -> TrasaT m (Result err a, view)
reform toForm prefix formlet = do 
  reformSingle toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view

reformSingle :: (MonadIO m, Monoid view)
  => ([(Text, Text)] -> view -> view)
  -> Text
  -> Form (TrasaT m) Text err view a
  -> TrasaT m (Result err a, view)
reformSingle toForm prefix formlet = do
  (View viewf, res') <- runForm (Environment env) (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])
  where
  env :: MonadIO m => FormId -> TrasaT m (Value Text)
  env formId = do
    QueryString queryString <- trasaQueryString <$> ask
    let val = HM.lookup (tshow formId) queryString
    case val of
      Nothing -> pure Missing
      Just QueryParamFlag -> pure Default -- ???
      Just (QueryParamSingle x) -> pure (Found x)
      Just (QueryParamList x) -> pure (Found $ T.intercalate ", " x) -- ???

