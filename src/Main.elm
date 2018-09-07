port module Main exposing (..)

import Array
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
  , tracks : Array.Array Track
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 100 False 0 "" (Array.fromList [])
  , getPlaylist
  )

type Msg
  = DurationChange Float
  | Ended Bool
  | Play Bool
  | PlayNext
  | PlayPause
  | PlayPrevious
  | PlaylistReceived (Result Http.Error (Array.Array Track))
  | Seek String
  | SelectTrack Int String
  | TimeUpdate Float

port durationChange : (Float -> msg) -> Sub msg
port ended : (Bool -> msg) -> Sub msg
port play : (Bool -> msg) -> Sub msg
port timeUpdate : (Float -> msg) -> Sub msg

port seek : String -> Cmd msg
port playPause : Bool -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DurationChange value ->
      ( { model | duration = value }
      , Cmd.none
      )

    Ended _ ->
      let (newIndex, newUrl) = getNextTrack model in
        ( { model | selectedIndex = newIndex, selectedUrl = newUrl }
        , Cmd.none
        )

    Play _ ->
      ( { model | isPlaying = True }
      , Cmd.none
      )

    PlayNext ->
      let (newIndex, newUrl) = getNextTrack model in
        ( { model | selectedIndex = newIndex, selectedUrl = newUrl }
        , Cmd.none
        )

    PlayPause ->
      ( { model | isPlaying = not model.isPlaying }
      , playPause (not model.isPlaying)
      )

    PlayPrevious ->
      let (newIndex, newUrl) = getPreviousTrack model in
        ( { model | selectedIndex = newIndex, selectedUrl = newUrl }
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

    Seek value ->
      ( { model | currentTime = (Maybe.withDefault 0 (String.toFloat value)) }
      , seek value
      )

    SelectTrack index url ->
      ( { model | selectedIndex = index, selectedUrl = url }
      , Cmd.none
      )

    TimeUpdate value ->
      ( { model | currentTime = value }
      , Cmd.none
      )

getNextTrack model =
  let newIndex = modBy (Array.length model.tracks) (model.selectedIndex + 1) in
  let newUrl = getTrackUrl model newIndex in
    (newIndex, newUrl)

getPreviousTrack model =
  let newIndex = modBy (Array.length model.tracks) (model.selectedIndex - 1) in
  let newUrl = getTrackUrl model newIndex in
    (newIndex, newUrl)

getTrackUrl model index =
  Array.get index model.tracks
    |> Maybe.map (\t -> t.location)
    |> Maybe.withDefault ""

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ durationChange DurationChange
    , ended Ended
    , play Play
    , timeUpdate TimeUpdate
    ]

view : Model -> Html Msg
view model =
  div []
  [ audio
    [ src model.selectedUrl
    , controls False
    , autoplay True
    , id "audio"
    ] []
  , div [ class "player-controls" ]
    [ button [ onClick PlayPause ] [ text (if model.isPlaying then "❚❚" else "►") ]
    , button [ onClick PlayPrevious ] [ text "<" ]
    , button [ onClick PlayNext ] [ text ">" ]
    , input
      [ type_ "range"
      , Html.Attributes.max (String.fromFloat model.duration)
      , value (String.fromFloat model.currentTime)
      , onInput Seek
      ] []
    ]
  , ul [] (Array.toList (rows model))
  ]

rows model =
  Array.map row (Array.indexedMap (\i t -> (i, t, model.selectedIndex)) model.tracks)

row (i, track, selectedIndex) =
  li
    [ classList [ ("selected", selectedIndex == i) ]
    , onClick (SelectTrack i track.location)
    ]
    [ text (track.creator ++ " - " ++ track.title) ]

getPlaylist =
  Http.send PlaylistReceived (Http.get "/roster-mellow.json" playlistDecoder)

playlistDecoder =
  Decode.field "playlist" (Decode.field "trackList" (Decode.field "track" (Decode.array trackDecoder)))

trackDecoder =
  Decode.map3 Track
    (Decode.field "creator" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "location" Decode.string)