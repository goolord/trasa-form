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

module Trasa.Form where

import Control.Arrow ((&&&))
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time
import GHC.Generics hiding (Meta)
import Lucid
import Text.Reform.Backend
import Text.Reform.Core
import qualified Text.Reform.Generalized as G
import Text.Reform.Result      (FormId, Result(Ok), unitRange)
import Topaz.Rec ((<:))
import Trasa.Core
import qualified Text.Read
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Trasa.Method as M

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
  encodeRoute :: Prepared route response -> Text

instance FormError Text where
  type ErrorInputType Text = Text
  commonFormError = T.pack . (commonFormErrorStr T.unpack)

data Foo = Foo Text Text Int
type SimpleForm a = Form Identity Text Text (Html ()) () a

readMay :: Read a => Text -> Maybe a
readMay = Text.Read.readMaybe . T.unpack

readInt :: Text -> Either Text Int
readInt input = case readMay input of
  Just int -> Right int
  Nothing -> Left "Not a number"

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
