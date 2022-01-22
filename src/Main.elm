port module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import ElmUiRenderer exposing (elmUiRenderer)
import Hovercraft exposing (Hovercraft, hovercraftDecoder)
import Html exposing (Html)
import Json.Decode as Json 
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

port requestSearch : String -> Cmd msg
port searchReceiver : (List Json.Value -> msg) -> Sub msg

type alias Model =
    { query : String, hits : List (Result Json.Error Hovercraft) }


type Msg
    = Change String
    | RecvSearch (List (Result Json.Error Hovercraft))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { query = "", hits = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newQuery ->
            ( { model
                | query = newQuery
              }
            , requestSearch newQuery
            )

        RecvSearch hits ->
            ( { model
                | hits = hits
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    searchReceiver (\hits -> RecvSearch (List.map (Json.decodeValue hovercraftDecoder) hits))


view : Model -> Html Msg
view model =
    layout [ padding 30 ] <|
        column [ spacing 7 ]
            (Input.text [ Input.focusedOnLoad ]
                { onChange = Change
                , text = model.query
                , placeholder = Just <| Input.placeholder [] <| text "Type here"
                , label = Input.labelAbove [] <| text "Text to search"
                }
                :: List.map viewHit model.hits
            )


viewHit : Result Json.Error Hovercraft -> Element Msg
viewHit hit =
    case hit of
        Ok hovercraft ->
            case Markdown.parse hovercraft.hover.contents.value of
                Ok contents ->
                    case Markdown.render elmUiRenderer contents of
                        Ok rendered ->
                            column
                                [ Border.width 2
                                , Border.rounded 6
                                , Border.color <| rgb255 0xC0 0xC0 0xC0
                                ]
                                (List.map (el [ paddingXY 10 0 ]) rendered)

                        Err err ->
                            text err

                Err err ->
                    column [] (List.map (\e -> text (Markdown.deadEndToString e)) err)
        Err err -> text (Json.errorToString err)
