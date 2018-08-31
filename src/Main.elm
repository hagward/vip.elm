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
  ( Model 0 [ (Track "creator" "title" "location"), (Track "creator2" "title2" "location") ]
  , Cmd.none
  )

type Msg = SelectTrack Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTrack track ->
      ( { model | selected = track }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
  div [] (rows model)

rows : Model -> List (Html msg)
rows model =
  model.tracks
    |> List.indexedMap Tuple.pair
    |> List.map (\(i, t) -> (i, t, model.selected))
    |> List.map row

row (i, track, selected) =
  if selected == i then
    div [ style "color" "green" ] [ text (track.creator ++ " - " ++ track.title) ]
  else
    div [] [ text (track.creator ++ " - " ++ track.title) ]