port module Main exposing (Model, Msg(..), Track, createTrackRegex, findTracks, formattedTime, getNextTrack, getPlaylist, getPreviousTrack, getTrackUrl, init, main, matchToTrack, padWithZero, playPause, row, rows, scrollToTrack, seek, subscriptions, toVipAersiaUrl, update, view, xmlToTracks)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Random
import Regex
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
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
    | Ended
    | Play
    | PlaylistReceived (Result Http.Error String)
    | PlayNext
    | PlayPause
    | PlayPrevious
    | PlayRandom Int
    | Seek String
    | SelectTrack Int String
    | TimeUpdate Float


port scrollToTrack : Int -> Cmd msg


port seek : String -> Cmd msg


port playPause : Bool -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DurationChange value ->
            ( { model | duration = value }
            , Cmd.none
            )

        Ended ->
            ( model
            , playRandomTrack model
            )

        Play ->
            ( { model | isPlaying = True }
            , Cmd.none
            )

        PlaylistReceived result ->
            case result of
                Ok xml ->
                    let
                        tracks =
                            xmlToTracks xml

                        newModel =
                            { model | tracks = tracks }
                    in
                    ( newModel
                    , playRandomTrack newModel
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
            , scrollToTrack newIndex
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
            , scrollToTrack newIndex
            )

        PlayRandom index ->
            let
                trackUrl =
                    getTrackUrl model index
            in
            ( { model | selectedIndex = index, selectedUrl = trackUrl }
            , scrollToTrack index
            )

        Seek value ->
            ( { model | currentTime = Maybe.withDefault 0 (String.toFloat value) }
            , seek value
            )

        SelectTrack index url ->
            ( { model | selectedIndex = index, selectedUrl = url }
            , scrollToTrack index
            )

        TimeUpdate value ->
            ( { model | currentTime = value }
            , Cmd.none
            )


playRandomTrack : Model -> Cmd Msg
playRandomTrack model =
    Random.generate PlayRandom (Random.int 0 (Array.length model.tracks - 1))


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
        Regex.fromString "<creator>(.+?)</creator>\\s+<title>(.+?)</title>\\s+<location>https?://(.+?)</location>"


matchToTrack : Regex.Match -> Track
matchToTrack match =
    let
        array =
            Array.fromList match.submatches

        creator =
            Array.get 0 array |> Maybe.withDefault (Just "") |> Maybe.withDefault ""

        title =
            Array.get 1 array |> Maybe.withDefault (Just "") |> Maybe.withDefault ""

        location =
            "https://" ++ (Array.get 2 array |> Maybe.withDefault (Just "") |> Maybe.withDefault "")
    in
    Track creator title location



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ audioPlayer model
        , playerControls model
        , playlist model
        ]


audioPlayer : Model -> Html Msg
audioPlayer model =
    if String.length model.selectedUrl > 0 then
        audio
            [ src model.selectedUrl
            , autoplay True
            , controls False
            , id "audio"
            , onDurationChange DurationChange
            , onEnded Ended
            , onPlay Play
            , onTimeUpdate TimeUpdate
            ]
            []

    else
        Html.text ""


playerControls : Model -> Html Msg
playerControls model =
    div [ class "player-controls" ]
        [ button [ class "player-control", onClick PlayPause ]
            [ text
                (if model.isPlaying then
                    "❚❚"

                 else
                    "►"
                )
            ]
        , button [ class "player-control", onClick PlayPrevious ] [ text "<" ]
        , button [ class "player-control", onClick PlayNext ] [ text ">" ]
        , div [ class "player-control seek" ]
            [ div [ class "timer" ] [ text (formattedTime model.currentTime) ]
            , input
                [ type_ "range"
                , Html.Attributes.max (String.fromFloat model.duration)
                , value (String.fromInt (floor model.currentTime))
                , onInput Seek
                ]
                []
            , div [ class "timer" ] [ text (formattedTime model.duration) ]
            ]
        ]


playlist : Model -> Html Msg
playlist model =
    ul [ class "playlist", id "playlist" ] (rows model)


onDurationChange : (Float -> msg) -> Attribute msg
onDurationChange msg =
    on "durationchange" <| Decode.map msg <| Decode.at [ "target", "duration" ] Decode.float


onEnded : msg -> Attribute msg
onEnded message =
    on "ended" (Decode.succeed message)


onPlay : msg -> Attribute msg
onPlay message =
    on "play" (Decode.succeed message)


onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" <| Decode.map msg <| Decode.at [ "target", "currentTime" ] Decode.float


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
    Http.get
    { url = toVipAersiaUrl "roster.xml"
    , expect = Http.expectString PlaylistReceived
    }



toVipAersiaUrl : String -> String
toVipAersiaUrl playlistFilename =
    Url.crossOrigin "https://vip.aersia.net" [ playlistFilename ] []
