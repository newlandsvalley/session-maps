module Sessions.MapComponent where

import Prelude (Unit, Void, ($), (<<<), (==), bind, discard, map, negate, pure, unit)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (cons, head)
import Data.Either (Either(..))
import Data.Map (empty, lookup, values)
import Data.List (toUnfoldable)
import Data.Foldable (sequence_)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML.Core (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import OpenLayers.Layer.Tile as Tile
import OpenLayers.Map as Map
import OpenLayers.Proj as Proj
import OpenLayers.Source.OSM as OSM
import OpenLayers.View as View
import Sessions.Data (SessionKey, SessionsMapping, 
        ukSessionsMapping, sessionKey)
import Sessions.Postcode (LonLat, loadGeoInfo)

type Slot = H.Slot (Const Void) Void

type State =  { map :: Maybe Map.Map         -- | The Map on the page
              , sessions :: SessionsMapping  -- | Mapping from session key to details
              }

type ChildSlots = ()


data Action
  = Initialize
  | Finalize
  | HandleChangeSession SessionKey

-- | The component definition
component :: forall q i o m . MonadAff m
          => H.Component HH.HTML q i o m
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
                     }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ renderMap state
      , renderSessionsMenu (toSessionKeys state.sessions)  ]

  --- The map is rendered by side-effct into the div tag whose id is 'map'
  --- This tag should have appropriate height and width designated in the css
  --- otherwise it won't display
  renderMap :: State -> H.ComponentHTML Action ChildSlots m
  renderMap state =
    HH.div_
       [ HH.div
           [ HP.id_ "parent" ]
           [ HH.text "map will go here" ]
       , HH.div [HP.id_ "map" ][]
       ]

  renderSessionsMenu :: Array SessionKey ->  H.ComponentHTML Action () m
  renderSessionsMenu sessions =
    let
      currentSession =
        fromMaybe "London: The Glastone Arms" $ head sessions
    in
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
                (Just <<<  HandleChangeSession)
            ]
            (cons
              (HH.option [ ] [ HH.text currentSession])
              (sessionOptions sessions currentSession)
            )
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

  handleAction ∷ Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      mymap <- createBasicMap edinburghLonLat
      H.modify_ (_ { map = Just mymap
                   , sessions = ukSessionsMapping
                   }
                )
    Finalize -> do
      pure unit

    HandleChangeSession key -> do
      state <- H.get
      let
        mSessionDetails = lookup key state.sessions
      case mSessionDetails of 
        Just details -> do 
          eLonLat <- H.liftAff $ loadGeoInfo details.postcode
          case eLonLat of 
            Left err -> 
              pure unit 
            Right lonLat -> do 
              _ <- H.liftEffect $ sequence_ $ map Map.clearTarget state.map
              newMap <- createBasicMap lonLat 
              H.modify_ (_ { map = Just newMap } )
          pure unit
        Nothing -> 
          pure unit
      pure unit

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

    -- Create the view of Edinburgh
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

toSessionKeys :: SessionsMapping -> Array SessionKey 
toSessionKeys sessionsMapping = 
  let 
    sessionsArray = toUnfoldable $ values sessionsMapping
  in
    map sessionKey sessionsArray

edinburghLonLat :: LonLat
edinburghLonLat = [-3.1883, 55.9533 ]

defaultZoom :: Number
defaultZoom = 15.0
