module Home exposing (main)

import Browser
import Browser.Events
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onEnter)
import Http
import Json.Decode as Decode
import Loading exposing (LoaderType(..), defaultConfig, render)



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


type alias Wind =
    { speed : Float }


type alias Condition =
    { rain : Maybe Float
    , snow : Maybe Float
    , clouds : Clouds
    }


type alias Clouds =
    { all : Maybe Float }


type alias Description =
    { short : String
    , long : String
    }


type alias Temperatures =
    { now : Float
    , min : Float
    , max : Float
    }


type alias MiscData =
    { humidity : Float
    , pressure : Float
    }


type alias Weather =
    { temperatures : Temperatures
    , other : MiscData
    , wind : Wind
    , description : Description
    , condition : Condition
    }


type Model
    = Landing Search
    | Loading
    | Success Weather
    | Failure String


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
    | RetrieveWeather String
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

        RetrieveWeather city ->
            ( Loading
            , Http.get
                { url = "https://api.openweathermap.org/data/2.5/find?q=" ++ city ++ "&units=imperial&type=accurate&APPID=7ab827fff3461690618eccf4312e5268"
                , expect = Http.expectJson GotWeather weatherDecoder
                }
            )

        GotWeather result ->
            case result of
                Ok decodedWeather ->
                    ( Success decodedWeather, Cmd.none )

                Err err ->
                    ( Failure (toString err), Cmd.none )

        Reset ->
            let
                resetSearch =
                    Search ""
            in
            ( Landing resetSearch, Cmd.none )


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Decode.map5 Weather
        (Decode.at [ "list" ] (Decode.index 0 temperatureDecoder))
        (Decode.at [ "list" ] (Decode.index 0 miscDataDecoder))
        (Decode.at [ "list" ] (Decode.index 0 windDecoder))
        (Decode.at [ "list" ] (Decode.index 0 descriptionDecoder))
        (Decode.at [ "list" ] (Decode.index 0 conditionDecoder))


temperatureDecoder : Decode.Decoder Temperatures
temperatureDecoder =
    Decode.map3 Temperatures
        (Decode.at [ "main", "temp" ] Decode.float)
        (Decode.at [ "main", "temp_min" ] Decode.float)
        (Decode.at [ "main", "temp_max" ] Decode.float)


miscDataDecoder : Decode.Decoder MiscData
miscDataDecoder =
    Decode.map2 MiscData
        (Decode.at [ "main", "humidity" ] Decode.float)
        (Decode.at [ "main", "pressure" ] Decode.float)


windDecoder : Decode.Decoder Wind
windDecoder =
    Decode.map Wind
        (Decode.at [ "wind", "speed" ] Decode.float)


descriptionDecoder : Decode.Decoder Description
descriptionDecoder =
    Decode.map2 Description
        (Decode.at [ "weather" ] (Decode.index 0 (Decode.at [ "main" ] Decode.string)))
        (Decode.at [ "weather" ] (Decode.index 0 (Decode.at [ "description" ] Decode.string)))


conditionDecoder : Decode.Decoder Condition
conditionDecoder =
    Decode.map3 Condition
        (Decode.field "rain" (Decode.nullable Decode.float))
        (Decode.field "snow" (Decode.nullable Decode.float))
        (Decode.field "clouds" cloudDecoder)


cloudDecoder : Decode.Decoder Clouds
cloudDecoder =
    Decode.map Clouds
        (Decode.field "all" (Decode.nullable Decode.float))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Landing search ->
            searchPage (inputSection search) "./assets/svg/undraw_location_search_bqps.svg" "0.65"

        Loading ->
            detailsPage loading "./assets/svg/undraw_unicorn_dp2f.svg" "1.0"

        Success weatherData ->
            detailsPage (weather weatherData) "./assets/svg/undraw_nature_fun_n9lv.svg" "1.0"

        Failure err ->
            detailsPage (failure err) "./assets/svg/undraw_server_down_s4lk.svg" "1.0"


searchPage : Html Msg -> String -> String -> Html Msg
searchPage innerHtml image opacity =
    div [ class "container mx-auto flex flex-col h-screen justify-center items-center" ]
        [ h1 [ style "font-family" "Vibes, cursive", style "color" "#475B63", class "text-4xl my-10" ] [ text "Spero Weather" ]
        , innerHtml
        , img [ src image, class "w-1/3 h-1/3 mt-20", style "opacity" opacity ] []
        ]


detailsPage : Html Msg -> String -> String -> Html Msg
detailsPage innerHtml image opacity =
    div [ class "container mx-auto flex flex-col justify-center items-center" ]
        [ h1 [ style "font-family" "Vibes, cursive", style "color" "#475B63", class "text-4xl my-10" ] [ text "Spero Weather" ]
        , innerHtml
        , img [ src image, class "w-1/3 h-1/3 mt-20", style "opacity" opacity ] []
        ]


inputSection : Search -> Html Msg
inputSection search =
    input
        [ class "w-1/2 appearance-none bg-gray-200 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
        , placeholder "city"
        , value search.city
        , onInput City
        , onEnter (RetrieveWeather search.city)
        ]
        []


weather : Weather -> Html Msg
weather weatherData =
    let
        newIcon =
            case weatherData.description.short of
                "Clouds" ->
                    "cloud"

                _ ->
                    "sun"

        windMessage =
            if weatherData.wind.speed < 10 then
                "meh"

            else if List.member (round weatherData.wind.speed) (List.range 10 30) then
                "interesting"

            else if List.member (round weatherData.wind.speed) (List.range 30 50) then
                "wow!"

            else
                "Run For Cover!"
    in
    section
        []
        [ div [ class "flex flex-row items-center justify-center" ]
            [ icon newIcon
            , span [ class "text-4xl ml-10", style "color" "#475B63" ] [ text weatherData.description.short ]
            ]
        , div [ style "color" "#475B63", class "text-5xl text-center my-5" ] [ text (toString weatherData.temperatures.now), text (" " ++ String.fromChar (Char.fromCode 176) ++ "F") ]
        , div [ class "flex flex-row items-center justify-center" ]
            [ icon "wind"
            , span [ class "text-4xl ml-10 my-5", style "color" "#475B63" ] [ text windMessage ]
            ]
        , div [ class "flex flex-row items-center justify-center my-5" ]
            [ icon "thermometer"
            , iconSmall "chevron-up"
            , span [ class "text-xl", style "color" "#475B63" ] [ text (toString weatherData.temperatures.max), text (" " ++ String.fromChar (Char.fromCode 176) ++ "F") ]
            , iconSmall "chevron-down"
            , span [ class "text-xl", style "color" "#475B63" ] [ text (toString weatherData.temperatures.min), text (" " ++ String.fromChar (Char.fromCode 176) ++ "F") ]
            ]
        , div [ class "flex flex-row items-center justify-center" ]
            [ icon "target"
            , span [ class "text-4xl ml-10", style "color" "#475B63" ] [ text (toString weatherData.other.pressure) ]
            ]
        , div [ class "flex flex-row items-center justify-center my-5" ]
            [ icon "droplet"
            , span [ class "text-4xl ml-10", style "color" "#475B63" ] [ text (toString weatherData.other.humidity ++ "%") ]
            ]
        , div [ class "text-center text-xl my-10", style "color" "#475B63" ] [ text ("so basically..." ++ weatherData.description.long) ]
        ]


icon : String -> Html Msg
icon iconName =
    let
        iconPath =
            "./assets/svg/" ++ iconName ++ ".svg"
    in
    img [ style "height" "100px", style "width" "100px", src iconPath ] []


iconSmall : String -> Html Msg
iconSmall iconSmallName =
    let
        iconPath =
            "./assets/svg/" ++ iconSmallName ++ ".svg"
    in
    img [ style "height" "30px", style "width" "30px", class "mx-10", src iconPath ] []


iconCenter : String -> Html Msg
iconCenter iconCenterName =
    let
        iconPath =
            "./assets/svg/" ++ iconCenterName ++ ".svg"
    in
    img [ style "color" "#475B63", style "height" "100px", style "width" "100px", class "mx-auto", src iconPath ] []


loading : Html Msg
loading =
    div []
        [ Loading.render
            Sonar
            { defaultConfig | color = "#E09F3E" }
            Loading.On
        ]


failure : String -> Html Msg
failure err =
    div [ style "color" "#DA2C38", class "text-xl my-5" ]
        [ text err ]
