module Sessions.Postcode 
  ( LonLat
  , loadGeoInfo) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Bifunctor (lmap)
import Partial.Unsafe (unsafePartial)
import Global (encodeURI)
import Data.HTTP.Method (Method(..))
import Effect.Console (log)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Affjax (defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (Json, decodeJson, JsonDecodeError, stringify, (.:))

-- | LonLat only allows 2 numbers - one each for longitude and latitude
type LonLat = Array Number

loadGeoInfo ::
  String
  -> Aff (Either String LonLat)
loadGeoInfo postcode = do
  let
    encodedPostcode = unsafePartial (fromJust (encodeURI postcode))
    url = "http://api.postcodes.io/postcodes/" <> encodedPostcode

  res <- request $ defaultRequest
    { url = url, method = Left GET, responseFormat = ResponseFormat.json }

  case res <#> _.body of
    Left err -> do
      _ <- liftEffect $ log $ "postcode failed to load: " <> url
      pure $ Left $ "failed to load from: " <> url
    Right body -> do
      _ <- liftEffect $ log $ "affjax response body: " <> (stringify body)
      pure $ lmap show $ decodeGeoInfo body


-- | decode the geo info we get back
decodeGeoInfo :: Json -> Either JsonDecodeError LonLat
decodeGeoInfo json = do
    obj <- decodeJson json
    result <- obj .: "result"
    longitude <- result .: "longitude"
    latitude <- result .: "latitude"
    pure $ [longitude, latitude]      


