module Home exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model
  = { city : String }


init : Model
init = Model ""



-- UPDATE


type Msg
  = City String


update : Msg -> Model -> Model
update msg model =
  case msg of
    City city ->
      { model | city = city }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container mx-auto flex flex-col h-screen justify-center items-center" ]
    [ h1 [ style "font-family" "Vibes, cursive", style "color" "#475B63", class "text-4xl my-10"] [text "Spero Weather" ],
    input [ class "w-1/2 appearance-none bg-gray-200 text-gray-700 border border-gray-200 rounded py-3 px-4 mb-3 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
            , placeholder "city"
            , value model.city
            , onInput City ] [],
    img [ src "./assets/svg/undraw_location_search_bqps.svg", class "w-1/3 h-1/3 mt-20", style "opacity" "0.65" ] []
    ]
