{-# LANGUAGE 
    ConstraintKinds
  , OverloadedStrings
  , TypeFamilies
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , StandaloneDeriving 
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Trasa.Form 
  ( FormData(..)
  , FormError(..)
  , TrasaFormT(..)
  , TrasaForm
  , TrasaFormError(..)
  , bodyFormData
  , liftParser
  , liftToForm
  , reform
  , reformPost
  , trasaFormView
  , textError
  )
  where

import Control.Applicative (Alternative(..))
import Control.Monad.Except
import Control.Monad.Reader
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Bifunctor
import Ditto.Backend
import Ditto.Core hiding (view)
import Ditto.Types
import Lucid
import Trasa.Core hiding (optional)
import Trasa.Server
import Trasa.Url
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Web.FormUrlEncoded as HTTP

instance FormError QueryParam TrasaFormError where
  commonFormError e = TrasaFormError (Just e) (commonFormErrorText encQP e)
    where
    encQP QueryParamFlag = "<flag>"
    encQP (QueryParamSingle x) = x
    encQP (QueryParamList xs) = tshow xs

instance FormInput QueryParam where
  type FileType QueryParam = ()
  getInputStrings (QueryParamSingle x) = [T.unpack x]
  getInputStrings (QueryParamList xs) = fmap T.unpack xs
  getInputStrings (QueryParamFlag) = []
  getInputFile _ = Left $ commonFormError $ (NoFileFound (QueryParamSingle "No support for file uploads") :: CommonFormError QueryParam)

data TrasaFormError = TrasaFormError
  { trasaFormErrorType :: Maybe (CommonFormError QueryParam)
  , trasaFormErrorText :: Text
  }

instance ToHtml TrasaFormError where
  toHtml = toHtml . trasaFormErrorText
  toHtmlRaw = toHtmlRaw . trasaFormErrorText

instance IsString TrasaFormError where
  fromString = TrasaFormError Nothing . fromString

textError :: Text -> TrasaFormError
textError = TrasaFormError Nothing

liftParser :: (Text -> Either Text a) -> (QueryParam -> Either TrasaFormError a)
liftParser f q = case q of
  QueryParamSingle x -> first textError $ f x
  QueryParamList [x] -> first textError $ f x
  QueryParamFlag -> Left $ textError "Unexpected query flag"
  QueryParamList [] -> Left $ textError "Unexpect empty query list"
  QueryParamList (_:_:_) -> Left $ textError "Unexpected query string list"

tshow :: Show a => a -> Text
tshow = T.pack . show

type TrasaForm a = Form (TrasaFormT IO) QueryParam TrasaFormError (Html ()) a

reform :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form (TrasaFormT m) QueryParam err view a  -- ^ the formlet
  -> TrasaT m (Result err a, view)
reform toForm prefix formlet = do 
  (View viewf, res) <- flip runReaderT Get $ getTrasaFormT $ runForm prefix formlet
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])

instance MonadIO m => Environment (TrasaT m) QueryParam where
  environment formId = do
    QueryString queryString <- trasaQueryString <$> ask
    let val = HM.lookup (encodeFormId formId) queryString
    case val of
      Nothing -> pure Missing
      Just x -> pure (Found x)

reformPost :: (MonadIO m, Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Maybe (FormData a)
  -> Form (TrasaFormT m) QueryParam err view a  -- ^ the formlet
  -> TrasaT m (Result err a, view)
reformPost toForm prefix formData formlet = do 
  (View viewf, res) <- flip runReaderT (Post $ getFormData <$> formData) $ getTrasaFormT $ runForm prefix formlet
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])

newtype FormData a = FormData { getFormData :: HM.HashMap Text [Text] }
  deriving (Monoid, Semigroup, Eq, Ord, Show)

bodyFormData :: BodyCodec (FormData a)
bodyFormData = BodyCodec
  (pure "application/x-www-form-urlencoded")
  (HTTP.urlEncodeAsFormStable . getFormData)
  (fmap (FormData . HTTP.unForm) . HTTP.urlDecodeForm)

newtype TrasaFormT m a = TrasaFormT
  { getTrasaFormT :: ReaderT FormType (TrasaT m) a }
  deriving (Monad, Applicative, Functor, Alternative)

instance MonadTrans (TrasaFormT) where
  lift = TrasaFormT . lift . lift

deriving instance Monad m => MonadReader FormType (TrasaFormT m)

data FormType = Get | Post (Maybe (HM.HashMap Text [Text]))

instance Monad m => Environment (TrasaFormT m) QueryParam where
  environment formId = ask >>= \case
    Post (Just urlEncoded) -> do
      case HM.lookup (encodeFormId formId) urlEncoded of
        Nothing -> pure Missing
        Just [] -> pure $ Found QueryParamFlag
        Just [x] -> pure $ Found $ QueryParamSingle x
        Just xs -> pure $ Found $ QueryParamList xs
    Post Nothing -> pure Default
    Get -> TrasaFormT $ lift $ do
      QueryString queryString <- trasaQueryString <$> ask
      case HM.lookup (encodeFormId formId) queryString of
        Nothing -> pure Missing
        Just x -> pure (Found x)

liftToForm :: Monad m => TrasaT m a -> TrasaFormT m a
liftToForm = TrasaFormT . lift

trasaFormView :: Monad m
  => Text
  -> Form (TrasaFormT m) QueryParam err view a
  -> TrasaT m view
trasaFormView name form = flip runReaderT Get $ getTrasaFormT $ viewForm name form

