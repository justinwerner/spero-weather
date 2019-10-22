module Home exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onEnter)
import Http
import Json.Decode as Decode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Search =
    { city : String }


type alias Weather =
    { temp : Float }


type Model
    = Landing Search
    | Loading
    | Success Weather
    | Failure


init : () -> ( Model, Cmd Msg )
init _ =
    let
        newSearch =
            Search ""
    in
    ( Landing newSearch, Cmd.none )



-- UPDATE


type Msg
    = City String
    | RetrieveWeather
    | GotWeather (Result Http.Error Weather)
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        City city ->
            let
                cityToSearch =
                    Search city
            in
            ( Landing cityToSearch, Cmd.none )

        RetrieveWeather ->
            ( Loading
            , Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectJson GotWeather weatherDecoder
                }
            )

        GotWeather result ->
            case result of
                Ok weather ->
                    ( Success weather, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        Reset ->
            let
                resetSearch =
                    Search ""
            in
            ( Landing resetSearch, Cmd.none )


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Decode.map Weather
        (Decode.field "temp" Decode.float)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Landing search ->
            div [ class "container mx-auto flex flex-col h-screen justify-center items-center" ]
                [ h1 [ style "font-family" "Vibes, cursive", style "color" "#475B63", class "text-4xl my-10" ] [ text "Spero Weather" ]
                , input
                    [ class "w-1/2 appearance-none bg-gray-200 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                    , placeholder "city"
                    , value search.city
                    , onInput City
                    , onEnter RetrieveWeather
                    ]
                    []
                , img [ src "./assets/svg/undraw_location_search_bqps.svg", class "w-1/3 h-1/3 mt-20", style "opacity" "0.65" ] []
                ]

        Loading ->
            div [] [ text "I am loading..." ]

        Success weather ->
            div [] [ text "Success" ]

        Failure ->
            div [] [ text "Houston..." ]
