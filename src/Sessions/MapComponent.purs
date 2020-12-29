module Sessions.MapComponent where

import Data.Array (head)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.List (toUnfoldable)
import Data.Map (empty, lookup, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import OpenLayers.Feature as Feature
import OpenLayers.Geom.Point as Point
import OpenLayers.Layer.Tile as Tile
import OpenLayers.Layer.Vector as VectorLayer
import OpenLayers.Map as Map
import OpenLayers.Proj as Proj
import OpenLayers.Source.OSM as OSM
import OpenLayers.Source.Vector as VectorSource
import OpenLayers.Style.Icon as Icon
import OpenLayers.Style.Style as Style
import OpenLayers.View as View
import Prelude (Unit, Void, ($), (<<<), (==), bind, discard, map, pure, unit)
import Sessions.Mapping (SessionDetails, SessionKey, SessionsMapping, ukSessionsMapping, readThroughCachedLonLat, sessionKey)
import Sessions.Postcode (LonLat)

type Slot = H.Slot (Const Void) Void

type State =  { map :: Maybe Map.Map         -- | The Map on the page
              , sessions :: SessionsMapping  -- | Mapping from session key to details
              , key :: SessionKey            -- | The currently selected session
              }

type ChildSlots = ()


data Action
  = Initialize
  | Finalize
  | HandleChangeSessionRequest SessionKey

data Query a =
    HandleChangeSession SessionKey a

-- | The component definition
component :: forall i o m . MonadAff m
          => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

  where


  initialState :: i -> State
  initialState ml =  { map : Nothing
                     , sessions : empty
                     , key: defaultSessionKey
                     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div       
      [ HP.id_ "session-maps"]
      [ renderMap state
      , renderSessionDetails $ lookup state.key state.sessions
      , renderSessionsMenu state.key (toSessionKeys state.sessions) 
       ]

  --- The map is rendered by side-effct into the div tag whose id is 'map'
  --- This tag should have appropriate height and width designated in the css
  --- otherwise it won't display
  renderMap :: State -> H.ComponentHTML Action ChildSlots m
  renderMap state =
    HH.div_
       [ HH.div
           [ HP.id_ "title" ]
           [ HH.h2_ [HH.text state.key ]]
       , HH.div [HP.id_ "map" ][]
       ]

  renderSessionsMenu :: SessionKey -> Array SessionKey ->  H.ComponentHTML Action () m
  renderSessionsMenu currentSession sessions =
    HH.div
      [ HP.class_ (H.ClassName "leftPanelComponent")]
      [ HH.label
         [ HP.class_ (H.ClassName "labelAlignment") ]
         [ HH.text "choose session: " ]
      , HH.select
          [ HP.class_ $ H.ClassName "selection"
          , HP.id_  "session-menu"
          , HP.value currentSession
          , HE.onValueChange
              (Just <<<  HandleChangeSessionRequest)
          ]
          (sessionOptions sessions currentSession)
      ]

  sessionOptions :: ∀ a b. Array SessionKey -> SessionKey -> Array (HTML a b)
  sessionOptions sessions currentSession =
    let
      f :: ∀ p ix. SessionKey -> HTML p ix
      f sesh =
        let
           disabled = (sesh == currentSession)
        in
          HH.option
            [ HP.disabled disabled ]
            [ HH.text sesh]
    in
      map f sessions

  renderSessionDetails :: Maybe SessionDetails -> H.ComponentHTML Action ChildSlots m
  renderSessionDetails mSessionDetails = 
    case mSessionDetails of 
      Just details ->
        HH.dl
          [ ]
          [ renderKV "street" details.street
          , renderKV "postcode" details.postcode
          , renderKV "start time" details.startTime
          , renderKV "schedule" details.schedule
          ]
      Nothing -> 
        HH.text ""

  
  -- render key-value pairs as rows in a definition list
  renderKV :: ∀ a s. String -> String -> H.ComponentHTML a s m
  renderKV k v =
    HH.div_
      [
      HH.dt_
        [ HH.text k ]
      , HH.dd_
        [ HH.text v ]
      ]

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do      
      let
        sessionKeys = toSessionKeys ukSessionsMapping 
        currentSession =
          fromMaybe defaultSessionKey $ head sessionKeys
      _ <- H.modify_ (_ { sessions = ukSessionsMapping })
      _ <- handleQuery (HandleChangeSession currentSession unit)
      pure unit

    Finalize -> do
      pure unit

    HandleChangeSessionRequest key -> do
      _ <- handleQuery (HandleChangeSession key unit)
      pure unit

  handleQuery :: forall a.
    MonadAff m =>
    Query a ->
    H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    HandleChangeSession key next -> do
      state <- H.get
      Tuple newSessions eLonLat <- H.liftAff $ readThroughCachedLonLat key state.sessions
      case eLonLat of 
        Left err -> 
          pure unit 
        Right lonLat -> do 
          let
            mSessionDetails = lookup key state.sessions
          -- clear the old map
          _ <- H.liftEffect $ sequence_ $ map Map.clearTarget state.map
          -- create the new one
          map0 <- createBasicMap lonLat 
          -- and add a layer which marks the centre
          map1 <- addIconMarker lonLat map0
          H.modify_ (_ { map = Just map1
                       , sessions =  newSessions
                       , key = key } )
      pure (Just next)

--
-- Creates a basic map
-- 
createBasicMap :: forall o m . MonadAff m
          => LonLat -> H.HalogenM State Action () o m Map.Map
createBasicMap lonLat = do

  mymap <- H.liftEffect $ do
    -- Use OpenStreetMap as a source
    osm <- OSM.create'
    tile <- Tile.create {source: osm}

    -- Create the view centered on lonLat
    view <- View.create { projection: Proj.epsg_3857
                        , center: Proj.fromLonLat lonLat (Just Proj.epsg_3857)
                        , zoom: defaultZoom
                        }

    log "creating map at div id map"

    -- Create the map and set up the controls, layers and view
    Map.create {
        target: Map.target.asId "map"
        , layers: Map.layers.asArray [ tile ]
        , view: view}

  -- Return with the map
  pure mymap

--
-- Create an icon marker at the latitude and longitude we need (initially at centre) 
-- and add it as a new layer to the map.
--
addIconMarker :: forall o m . MonadAff m
          => LonLat -> Map.Map -> H.HalogenM State Action () o m Map.Map
addIconMarker lonLat mymap = 
  H.liftEffect $ do 

    --- we can also use fill and stroke against the icon if we prefer
    ---
    --- pfill <- Fill.create { color: Fill.color.asString "#c73326" }
    --- pstroke <- Stroke.create { color: Stroke.color.asString "white", width: 2}    

    picon <- Icon.create { anchor : [ 0.5, 46.0 ]
                         , anchorXUnits : "fraction"
                         , anchorYUnits : "pixels"
                         , scale : 0.025
                         , src : "assets/images/markerpin.png"       
                         }
                             
    pstyle <- Style.create {image: picon}

    point <- Point.create 
      (Proj.fromLonLat lonLat (Just Proj.epsg_3857))
      Nothing
    marker <- Feature.create $ Feature.Properties { geometry: point, name: "session" }

    Feature.setStyle (Just pstyle) marker

    vs <- VectorSource.create { features: VectorSource.features.asArray [marker] }
    layer <- VectorLayer.create { source : vs } 
    log "adding icon marker layer to map"

    _ <- Map.addLayer layer mymap
    pure mymap      

--
-- Ditto, but using a graphics circle as the marker
--
{-
addCircleMarker :: forall o m . MonadAff m
          => LonLat -> Map.Map -> H.HalogenM State Action () o m Map.Map
addCircleMarker lonLat mymap = 
  H.liftEffect $ do 

    pfill <- Fill.create { color: Fill.color.asString "#c73326" }
    pstroke <- Stroke.create { color: Stroke.color.asString "white", width: 2}
    pcircle <- Circle.create { radius: 8.0
                              , fill: pfill
                              , stroke: pstroke
                              }
    pstyle <- Style.create {image: pcircle}

    point <- Point.create 
      (Proj.fromLonLat lonLat (Just Proj.epsg_3857))
      Nothing
    marker <- Feature.create $ Feature.Properties { geometry: point, name: "session" }

    Feature.setStyle (Just pstyle) marker

    vs <- VectorSource.create { features: VectorSource.features.asArray [marker] }
    layer <- VectorLayer.create { source : vs } 
    log "adding circle graphic marker layer to map"

    _ <- Map.addLayer layer mymap
    pure mymap   
-}

toSessionKeys :: SessionsMapping -> Array SessionKey 
toSessionKeys sessionsMapping = 
  let 
    sessionsArray = toUnfoldable $ values sessionsMapping
  in
    map sessionKey sessionsArray

defaultZoom :: Number
defaultZoom = 17.0

defaultSessionKey :: SessionKey
defaultSessionKey = 
  "London: The Glastone Arms"
