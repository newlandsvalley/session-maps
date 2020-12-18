module Sessions.Data 
  ( SessionDetails
  , SessionKey
  , SessionsMapping
  , ukSessions
  , ukSessionsMapping
  , sessionKey ) where

import Prelude ((<>), ($), map)
import Data.Maybe (Maybe(..))
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Tuple (Tuple(..), fst, snd)
import Sessions.Postcode (LonLat)

type SessionKey = String
type SessionsMapping = Map SessionKey SessionDetails
  
type SessionDetails =
  {  city :: String 
  ,  street :: String 
  ,  postcode :: String 
  ,  venue :: String 
  ,  startTime :: String 
  ,  occurrence :: String
  ,  mLonLat :: Maybe LonLat
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

setLonLat :: LonLat -> SessionKey -> SessionsMapping -> SessionsMapping
setLonLat lonLat key mapping = 
  case lookup key mapping of 
    Just details -> 
      let 
        newDetails = details { mLonLat = Just lonLat }
      in 
        insert key newDetails mapping
    Nothing -> 
      mapping
  

gladstoneLondon :: SessionDetails
gladstoneLondon = 
  {  city : "London"
  ,  street : "64 Lant Street"
  ,  postcode : "SE1 1QN"
  ,  venue : "The Gladstone Arms" 
  ,  startTime : "8.00 p.m."
  ,  occurrence : "Monthly, Thursdays"
  ,  mLonLat : Nothing
  }

greenManLondon :: SessionDetails
greenManLondon = 
  {  city : "London"
  ,  street : "6 Riding House Street"
  ,  postcode : "W1W 7EP"
  ,  venue : "The Green Man" 
  ,  startTime : "7.00 p.m."
  ,  occurrence : "Monthly, Tuesdays"
  ,  mLonLat : Nothing
  }
 
sofisEdinburgh :: SessionDetails
sofisEdinburgh = 
  {  city : "Edinburgh"
  ,  street : "42-44 Buccleuch Street"
  ,  postcode : "EH8 9LP"
  ,  venue : "Sofi’s Southside" 
  ,  startTime : "2.00 p.m."
  ,  occurrence : "Every fourth Sunday"
  ,  mLonLat : Nothing
  } 