import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Track =
  { creator : String
  , title : String
  , location : String
  }

type alias Model =
  { selectedIndex : Int
  , selectedUrl : String
  , tracks : List Track
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 "" []
  , getPlaylist
  )

type Msg
  = SelectTrack Int String
  | PlaylistReceived (Result Http.Error (List Track))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTrack index url ->
      ( { model | selectedIndex = index, selectedUrl = url }
      , Cmd.none
      )
    PlaylistReceived result ->
      case result of
        Ok tracks ->
          ( { model | tracks = tracks }
          , Cmd.none
          )

        Err _ ->
          ( model
          , Cmd.none
          )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
  div []
  [ audio [ src model.selectedUrl, controls True ] []
  , div [] (rows model)
  ]

rows model =
  model.tracks
    |> List.indexedMap Tuple.pair
    |> List.map (\(i, t) -> (i, t, model.selectedIndex))
    |> List.map row

row (i, track, selectedIndex) =
  div
    [ classList [ ("selected", selectedIndex == i) ]
    , onClick (SelectTrack i track.location)
    ]
    [ text (track.creator ++ " - " ++ track.title) ]

getPlaylist =
  Http.send PlaylistReceived (Http.get "/roster-mellow.json" playlistDecoder)

playlistDecoder =
  Decode.field "playlist" (Decode.field "trackList" (Decode.field "track" (Decode.list trackDecoder)))

trackDecoder =
  Decode.map3 Track
    (Decode.field "creator" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "location" Decode.string)