port module Main exposing (Model, Msg(..), Track, createTrackRegex, durationChange, ended, findTracks, formattedTime, getNextTrack, getPlaylist, getPreviousTrack, getTrackUrl, init, main, matchToTrack, padWithZero, play, playPause, row, rows, scrollToTrack, seek, subscriptions, timeUpdate, toVipAersiaUrl, update, view, xmlToTracks)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Random
import Regex
import Url.Builder as Url



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 100 False 0 "" (Array.fromList [])
    , getPlaylist
    )



-- UPDATE


type Msg
    = DurationChange Float
    | Ended Bool
    | Play Bool
    | PlaylistReceived (Result Http.Error String)
    | PlayNext
    | PlayPause
    | PlayPrevious
    | PlayRandom Int
    | Seek String
    | SelectTrack Int String
    | TimeUpdate Float


port durationChange : (Float -> msg) -> Sub msg


port ended : (Bool -> msg) -> Sub msg


port play : (Bool -> msg) -> Sub msg


port timeUpdate : (Float -> msg) -> Sub msg


port scrollToTrack : () -> Cmd msg


port seek : String -> Cmd msg


port playPause : Bool -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DurationChange value ->
            ( { model | duration = value }
            , Cmd.none
            )

        Ended _ ->
            ( model
            , Random.generate PlayRandom (Random.int 0 (Array.length model.tracks - 1))
            )

        Play _ ->
            ( { model | isPlaying = True }
            , Cmd.none
            )

        PlaylistReceived result ->
            case result of
                Ok xml ->
                    let
                        tracks =
                            xmlToTracks xml
                    in
                    ( { model | tracks = tracks }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        PlayNext ->
            let
                ( newIndex, newUrl ) =
                    getNextTrack model
            in
            ( { model | selectedIndex = newIndex, selectedUrl = newUrl }
            , scrollToTrack ()
            )

        PlayPause ->
            ( { model | isPlaying = not model.isPlaying }
            , playPause (not model.isPlaying)
            )

        PlayPrevious ->
            let
                ( newIndex, newUrl ) =
                    getPreviousTrack model
            in
            ( { model | selectedIndex = newIndex, selectedUrl = newUrl }
            , scrollToTrack ()
            )

        PlayRandom index ->
            let
                trackUrl =
                    getTrackUrl model index
            in
            ( { model | selectedIndex = index, selectedUrl = trackUrl }
            , scrollToTrack ()
            )

        Seek value ->
            ( { model | currentTime = Maybe.withDefault 0 (String.toFloat value) }
            , seek value
            )

        SelectTrack index url ->
            ( { model | selectedIndex = index, selectedUrl = url }
            , scrollToTrack ()
            )

        TimeUpdate value ->
            ( { model | currentTime = value }
            , Cmd.none
            )


getNextTrack : Model -> ( Int, String )
getNextTrack model =
    let
        newIndex =
            modBy (Array.length model.tracks) (model.selectedIndex + 1)
    in
    let
        newUrl =
            getTrackUrl model newIndex
    in
    ( newIndex, newUrl )


getPreviousTrack : Model -> ( Int, String )
getPreviousTrack model =
    let
        newIndex =
            modBy (Array.length model.tracks) (model.selectedIndex - 1)
    in
    let
        newUrl =
            getTrackUrl model newIndex
    in
    ( newIndex, newUrl )


getTrackUrl : Model -> Int -> String
getTrackUrl model index =
    Array.get index model.tracks
        |> Maybe.map (\t -> t.location)
        |> Maybe.withDefault ""


xmlToTracks : String -> Array.Array Track
xmlToTracks s =
    findTracks s
        |> List.map matchToTrack
        |> Array.fromList


findTracks : String -> List Regex.Match
findTracks s =
    Regex.find createTrackRegex s


createTrackRegex : Regex.Regex
createTrackRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "<creator>(.+?)</creator>\\s+<title>(.+?)</title>\\s+<location>(.+?)</location>"


matchToTrack : Regex.Match -> Track
matchToTrack match =
    let
        array =
            Array.fromList match.submatches
    in
    Track
        (Array.get 0 array |> Maybe.withDefault (Just "") |> Maybe.withDefault "")
        (Array.get 1 array |> Maybe.withDefault (Just "") |> Maybe.withDefault "")
        (Array.get 2 array |> Maybe.withDefault (Just "") |> Maybe.withDefault "")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ durationChange DurationChange
        , ended Ended
        , play Play
        , timeUpdate TimeUpdate
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ audio
            [ src model.selectedUrl
            , controls False
            , autoplay True
            , id "audio"
            ]
            []
        , div [ class "player-controls" ]
            [ button [ onClick PlayPause ]
                [ text
                    (if model.isPlaying then
                        "❚❚"

                     else
                        "►"
                    )
                ]
            , button [ onClick PlayPrevious ] [ text "<" ]
            , button [ onClick PlayNext ] [ text ">" ]
            , div [ class "timer" ] [ text (formattedTime model.currentTime) ]
            , input
                [ type_ "range"
                , Html.Attributes.max (String.fromFloat model.duration)
                , value (String.fromInt (floor model.currentTime))
                , onInput Seek
                ]
                []
            , div [ class "timer" ] [ text (formattedTime model.duration) ]
            ]
        , ul [] (rows model)
        ]


formattedTime : Float -> String
formattedTime time =
    let
        minutes =
            floor (time / 60) |> String.fromInt |> padWithZero
    in
    let
        seconds =
            modBy 60 (floor time) |> String.fromInt |> padWithZero
    in
    minutes ++ ":" ++ seconds


padWithZero : String -> String
padWithZero s =
    if String.length s == 1 then
        "0" ++ s

    else
        s


rows : Model -> List (Html Msg)
rows model =
    Array.indexedMap (\i t -> ( i, t, model.selectedIndex )) model.tracks
        |> Array.map row
        |> Array.toList


row : ( Int, Track, Int ) -> Html Msg
row ( i, track, selectedIndex ) =
    li
        [ classList [ ( "selected", selectedIndex == i ) ]
        , onClick (SelectTrack i track.location)
        ]
        [ text (track.creator ++ " - " ++ track.title) ]



-- HTTP


getPlaylist : Cmd Msg
getPlaylist =
    Http.send PlaylistReceived (Http.getString (toVipAersiaUrl "roster.xml"))


toVipAersiaUrl : String -> String
toVipAersiaUrl playlist =
    Url.crossOrigin "https://vip.aersia.net" [ playlist ] []
