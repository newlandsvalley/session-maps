module Sessions.Mapping 
  ( SessionDetails
  , SessionKey
  , SessionsMapping
  , ukSessions
  , ukSessionsMapping
  , readThroughCachedLonLat
  , setLonLat
  , sessionKey ) where

import Prelude ((<>), ($),bind, map, pure)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Sessions.Postcode (LonLat, loadGeoInfo)

type SessionKey = String
type SessionsMapping = Map SessionKey SessionDetails
  
type SessionDetails =
  {  city :: String 
  ,  street :: String 
  ,  postcode :: String 
  ,  venue :: String 
  ,  startTime :: String 
  ,  schedule :: String
  ,  eLonLat :: Either String LonLat
  }

sessionKey :: SessionDetails -> SessionKey
sessionKey details = 
  details.city <> ": " <> details.venue

ukSessionsMapping :: SessionsMapping
ukSessionsMapping =
  let
    f :: SessionDetails -> Tuple SessionKey SessionDetails
    f sessionDetails = Tuple (sessionKey sessionDetails) sessionDetails
  in
    fromFoldable $ map f ukSessions

ukSessions :: Array SessionDetails
ukSessions = 
  [ gladstoneLondon
  , greenManLondon
  , sofisEdinburgh
  ]

setLonLat :: 
  (Either String LonLat) 
  -> SessionKey 
  -> SessionsMapping 
  -> SessionsMapping
setLonLat eLonLat key mapping = 
  case lookup key mapping of 
    Just details -> 
      let 
        newDetails = details { eLonLat = eLonLat }
      in 
        insert key newDetails mapping
    Nothing -> 
      mapping

-- read through the mapping for the reuested session and return 
-- both the (possibly updated) mapping and the LonLat geoInfo 
-- (if we find it)
readThroughCachedLonLat :: 
  forall m . MonadAff m 
  => SessionKey 
  -> SessionsMapping 
  -> m (Tuple SessionsMapping (Either String LonLat))
readThroughCachedLonLat key mapping = 
  case lookup key mapping of
    -- the session key exists
    Just details -> 
      case details.eLonLat of 
        -- there is no geoinfo loaded yet so load and cache it
        Left _ -> do
          eLonLat <- liftAff $ loadGeoInfo details.postcode
          let 
            newMapping = setLonLat eLonLat key mapping
          pure $ Tuple newMapping eLonLat
        -- it's already there
        Right lonLat ->
          pure $ Tuple mapping details.eLonLat
    -- no session key exists (shouldn't happen)
    Nothing ->
      pure $ Tuple mapping (Left "could not find session key")

  

gladstoneLondon :: SessionDetails
gladstoneLondon = 
  {  city : "London"
  ,  street : "64 Lant Street"
  ,  postcode : "SE1 1QN"
  ,  venue : "The Gladstone Arms" 
  ,  startTime : "8.00 p.m."
  ,  schedule : "Monthly, Thursdays"
  ,  eLonLat :  Left "not loaded"
  }

greenManLondon :: SessionDetails
greenManLondon = 
  {  city : "London"
  ,  street : "6 Riding House Street"
  ,  postcode : "W1W 7EP"
  ,  venue : "The Green Man" 
  ,  startTime : "7.00 p.m."
  ,  schedule : "Monthly, Tuesdays"
  ,  eLonLat :  Left "not loaded"
  }
 
sofisEdinburgh :: SessionDetails
sofisEdinburgh = 
  {  city : "Edinburgh"
  ,  street : "42-44 Buccleuch Street"
  ,  postcode : "EH8 9LP"
  ,  venue : "Sofi’s Southside" 
  ,  startTime : "2.00 p.m."
  ,  schedule : "Every fourth Sunday"
  ,  eLonLat : Left "not loaded"
  } 