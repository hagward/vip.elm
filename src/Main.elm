import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = SelectRow Int

update msg model =
  case msg of
    SelectRow a ->
      a

view model =
  div []
    [ row model 1 "One" SelectRow
    , row model 2 "Two" SelectRow
    , row model 3 "Three" SelectRow
    , row model 4 "Four" SelectRow
    , row model 5 "Five" SelectRow
    ]

row model n t select =
  if model == n then
    div [ style "color" "green", onClick (select n) ] [ text t ]
  else
    div [ onClick (select n) ] [ text t ]