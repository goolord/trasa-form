{-# language DataKinds #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language PolyKinds #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity
import Data.Kind (Type)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Trasa.Core
import Text.Reform.Core hiding (view)
import Trasa.Server
import Trasa.Form
import Lucid
import Text.Reform (Result(..))
import Data.Text (Text)
import qualified Control.Applicative
import qualified Text.Read
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B
import qualified Trasa.Method as Method

data FooB = FooB Int Int
  deriving Show

-- Our route data type. We define this ourselves.
data Route :: [Type] -> [Param] -> Bodiedness -> Type -> Type where
  HelloWorld :: Route 
    '[ByteString] -- ^ now the path captures the first piece as a ByeString
    '[('Optional ByteString)] -- ^ there is an optional query parameter, decoded as a ByteString
    'Bodyless -- ^ the route does not have a request body
    ByteString -- ^ the response body will be `ByteString`
  FormTest :: Route
    '[]
    '[ 'Optional Text, 'Optional Int, 'Optional Text, 'Optional Int ]
    'Bodyless
    (Html ())

bodyText :: BodyCodec ByteString
bodyText = BodyCodec 
  (pure "text/html; charset=utf-8") -- ^ NonEmpty list of the HTTP media type names.
  id -- ^ encode from ByteString to ByteString
  Right -- ^ decode from ByteString to (Either Text ByteString)

bodyHtml :: BodyCodec (Html a)
bodyHtml = BodyCodec (pure "text/html;charset=utf-8") renderBS (const (Left "can not decode html"))

bytestring :: CaptureCodec ByteString
bytestring = CaptureCodec (TE.decodeUtf8 . B.toStrict) (Just . B.fromStrict . TE.encodeUtf8)

text :: CaptureCodec Text
text = CaptureCodec id Just

showReadCodec = CaptureCodec tshow (Text.Read.readMaybe . T.unpack)

int :: CaptureCodec Int
int = showReadCodec

-- | metadata about our routes: value level functions and data for constructing
--   and decoding paths
meta :: Route captures queries request response -> MetaCodec captures queries request response
meta route = case route of
  HelloWorld -> Meta 
    (capture bytestring ./ end) -- ^ match "/hello"
    (optional "b" bytestring .& qend) -- ^ no query parameters
    bodyless -- ^ no request body
    (resp (one bodyText)) -- ^ response body is one BodyCodec: our bodyText function above
    Method.get -- ^ http method: GET
  FormTest -> Meta
    (match "test" ./ end)
    (optional "t1" text .& optional "i1" int .& optional "t2" text .& optional "i2" int .& qend)
    bodyless
    (resp (one bodyHtml))
    Method.get

-- | this function defines how we handle routes with our web server:
--   what actions we perform based on the route and its captures & queries
routes
  :: forall captures queries request response.
     Route captures queries request response -- ^ our route GADT, polymorphic over its type variables
  -> Rec Identity captures -- ^ an extensible record of the captures for this route
  -> Rec Parameter queries -- ^ an extensible record of the captures for this route
  -> RequestBody Identity request -- ^ the request body
  -> TrasaT IO response -- ^ our response
routes route captures queries reqBody = case route of
  HelloWorld -> go helloWorld
  FormTest -> go formTest
  where
  -- | this helper function uses the `handler` function to unwrap the `Arguments` type family.
  go :: Arguments captures queries request (TrasaT IO response) -> TrasaT IO response
  go f = handler captures queries reqBody f

helloWorld :: ByteString -> Maybe ByteString -> TrasaT IO ByteString
helloWorld a (Just b) = pure $ a <> ", " <> b <> "!"
helloWorld a Nothing = pure a

type family QueryArguments (querys :: [Param]) (result :: Type) :: Type where
  QueryArguments '[] r = r
  QueryArguments (q ': qs) r = ParamBase q -> QueryArguments qs r

-- formArgs :: Monad f => Rec Parameter qrys -> QueryArguments qrys (f (Rec Parameter qrys))
-- formArgs queries args = do
--   pure RecNil

formFooB :: Monad f => Form (TrasaT IO) Text Text (HtmlT f ()) () FooB
formFooB = FooB 
  <$> childErrors ++> label "Int Field 1" ++> inputInt readInt 0
  <*> childErrors ++> label "Int Field 2" ++> inputInt readInt 0
  <*  buttonSubmit (const (Right mempty)) "" ("Submit" :: Text)

prepare :: Route captures query request response -> Arguments captures query request (Prepared Route response)
prepare = prepareWith meta

link :: Prepared Route response -> Url
link = (linkWith (mapMeta captureEncoding captureEncoding id id . meta))

formTest :: Maybe Text -> Maybe Int -> Maybe Text -> Maybe Int -> TrasaT IO (Html ())
formTest mt1 mi1 mt2 mi2 = do
  (res, html) <- simpleReform (link (prepare FormTest Nothing Nothing Nothing Nothing)) formFooB
  defaultLayout $ do
    case res of
      Ok x -> toHtml $ show x
      Error _ -> mempty
    html

defaultLayout :: Html () -> TrasaT IO (Html ())
defaultLayout children = do
  pure $ html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "https://unpkg.com/sakura.css/css/sakura.css", type_ "text/css"]
    body_ $ do
      children

-- | We define a list of all the routes for our server for wai
allRoutes :: [Constructed Route]
allRoutes = [Constructed HelloWorld, Constructed FormTest]

-- | Another implimentaiton detail: this creates the data structure used to do routing
router :: Router Route
router = routerWith (mapMeta captureDecoding captureDecoding id id . meta) allRoutes

-- | `wai` application
application :: Application
application = serveWith
  (metaCodecToMetaServer . meta) -- ^ implimentaiton detail: this just marshals some types
  routes -- ^ routes function defined above
  router -- ^ router function defined above

main :: IO ()
main = run 8080 (logStdoutDev application)

