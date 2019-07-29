{-# language ConstraintKinds #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trasa.Form 
  ( reform
  , reformQP
  , reformPost
  , liftParser
  , TrasaForm
  , TrasaSimpleForm
  , FormError(..)
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Ditto.Backend
import Ditto.Core hiding (view)
import Ditto.Result
import Lucid
import Trasa.Core hiding (optional)
import Trasa.Server
import Trasa.Url
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Web.FormUrlEncoded as HTTP

instance FormError QueryParam Text where
  commonFormError = commonFormErrorText encQP
    where
    encQP QueryParamFlag = "<flag>"
    encQP (QueryParamSingle x) = x
    encQP (QueryParamList xs) = tshow xs

liftParser :: (Text -> Either Text a) -> (QueryParam -> Either Text a)
liftParser f q = case q of
  QueryParamSingle x -> f x
  QueryParamList [x] -> f x
  QueryParamFlag -> Left "Unexpected query flag"
  QueryParamList [] -> Left "Unexpect empty query list"
  QueryParamList (_:_:_) -> Left "Unexpected query string list"

tshow :: Show a => a -> Text
tshow = T.pack . show

type TrasaSimpleForm a = Form (TrasaT IO) Text Text (Html ()) a
type TrasaForm a = Form (TrasaT IO) QueryParam Text (Html ()) a

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

reformQP :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form (TrasaT m) QueryParam err view a  -- ^ the formlet
  -> TrasaT m (Result err a, view)
reformQP toForm prefix formlet = do 
  reformSingleQP toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view

reformSingleQP :: (MonadIO m, Monoid view)
  => ([(Text, Text)] -> view -> view)
  -> Text
  -> Form (TrasaT m) QueryParam err view a
  -> TrasaT m (Result err a, view)
reformSingleQP toForm prefix formlet = do
  (View viewf, res') <- runForm (Environment env) (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])
  where
  env :: MonadIO m => FormId -> TrasaT m (Value QueryParam)
  env formId = do
    QueryString queryString <- trasaQueryString <$> ask
    let val = HM.lookup (tshow formId) queryString
    case val of
      Nothing -> pure Missing
      Just x -> pure (Found x)

reformPost :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> BS.ByteString
  -> Form (TrasaT m) QueryParam err view a  -- ^ the formlet
  -> TrasaT m (Result err a, view)
reformPost toForm prefix reqBody formlet = do 
  reformSinglePost toForm' prefix reqBody formlet
  where
  toForm' hidden view = toForm (("formname",prefix) : hidden) view

reformSinglePost :: (MonadIO m, Monoid view)
  => ([(Text, Text)] -> view -> view)
  -> Text
  -> BS.ByteString
  -> Form (TrasaT m) QueryParam err view a
  -> TrasaT m (Result err a, view)
reformSinglePost toForm prefix reqBody formlet = do
  let formData = parseRequestBody reqBody
  (View viewf, res') <- runForm (Environment $ env formData) (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])
  where
  env :: MonadIO m => HM.HashMap Text [Text] -> FormId -> TrasaT m (Value QueryParam)
  env multipart formId = do
    let val = HM.lookup (tshow formId) multipart
    case val of
      Nothing -> pure Missing
      Just [] -> pure $ Found QueryParamFlag
      Just [x] -> pure $ Found $ QueryParamSingle x
      Just xs -> pure $ Found $ QueryParamList xs

parseRequestBody :: BS.ByteString -> HM.HashMap Text [Text]
parseRequestBody reqBody = case HTTP.urlDecodeForm (BSL.fromStrict reqBody) of
  Left _ -> HM.empty
  Right (HTTP.Form formData) -> formData
