port module Main exposing (..)

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
  { currentTime : Float
  , duration : Float
  , isPlaying : Bool
  , selectedIndex : Int
  , selectedUrl : String
  , tracks : List Track
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 100 False 0 "" []
  , getPlaylist
  )

type Msg
  = DurationChange Float
  | Play Bool
  | PlayPause
  | PlaylistReceived (Result Http.Error (List Track))
  | Seek String
  | SelectTrack Int String

port durationChange : (Float -> msg) -> Sub msg
port play : (Bool -> msg) -> Sub msg

port seek : String -> Cmd msg
port playPause : Bool -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DurationChange value ->
      ( { model | duration = value }
      , Cmd.none
      )

    Play _ ->
      ( { model | isPlaying = True }
      , Cmd.none
      )

    PlayPause ->
      ( { model | isPlaying = not model.isPlaying }
      , playPause (not model.isPlaying)
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

    Seek value ->
      ( { model | currentTime = (Maybe.withDefault 0 (String.toFloat value)) }
      , seek value
      )

    SelectTrack index url ->
      ( { model | selectedIndex = index, selectedUrl = url }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ durationChange DurationChange
    , play Play
    ]

view : Model -> Html Msg
view model =
  div []
  [ audio
    [ src model.selectedUrl
    , controls True
    , autoplay True
    , id "audio"
    ] []
  , div [ class "player-controls" ]
    [ button [ onClick PlayPause ] [ text (if model.isPlaying then "❚❚" else "►") ]
    , input
      [ type_ "range"
      , Html.Attributes.max (String.fromFloat model.duration)
      , value (String.fromFloat model.currentTime)
      , onInput Seek
      ] []
    ]
  , ul [] (rows model)
  ]

rows model =
  model.tracks
    |> List.indexedMap Tuple.pair
    |> List.map (\(i, t) -> (i, t, model.selectedIndex))
    |> List.map row

row (i, track, selectedIndex) =
  li
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