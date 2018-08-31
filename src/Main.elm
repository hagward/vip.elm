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
  { selected : Int
  , tracks : List Track
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 []
  , getPlaylist
  )

type Msg
  = SelectTrack Int
  | PlaylistReceived (Result Http.Error (List Track))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTrack track ->
      ( { model | selected = track }
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
  div [] (rows model)

rows model =
  model.tracks
    |> List.indexedMap Tuple.pair
    |> List.map (\(i, t) -> (i, t, model.selected))
    |> List.map row

row (i, track, selected) =
  if selected == i then
    div [ style "color" "green", onClick (SelectTrack i) ] [ text (track.creator ++ " - " ++ track.title) ]
  else
    div [ onClick (SelectTrack i) ] [ text (track.creator ++ " - " ++ track.title) ]

getPlaylist =
  Http.send PlaylistReceived (Http.get "http://localhost:12345/roster.json" playlistDecoder)

playlistDecoder =
  Decode.field "playlist" (Decode.field "trackList" (Decode.field "track" (Decode.list trackDecoder)))

trackDecoder =
  Decode.map3 Track
    (Decode.field "creator" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "location" Decode.string)