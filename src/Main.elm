port module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import ElmUiRenderer exposing (elmUiRenderer)
import Hovercraft exposing (Entry, entryDecoder)
import Html exposing (Html)
import Json.Decode as Json
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


port requestSearch : ( Bool, String ) -> Cmd msg


port searchReceiver : (List Json.Value -> msg) -> Sub msg


type alias Model =
    { query : String, isFuzzMode : Bool, hits : List (Result Json.Error Entry) }


type Msg
    = Change String
    | FuzzMode Bool
    | RecvSearch (List (Result Json.Error Entry))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { query = "", isFuzzMode = False, hits = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newQuery ->
            ( { model
                | query = newQuery
              }
            , requestSearch ( model.isFuzzMode, newQuery )
            )

        FuzzMode flag ->
            ( { model
                | isFuzzMode = flag
              }
            , requestSearch ( flag, model.query )
            )

        RecvSearch hits ->
            ( { model
                | hits = hits
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    searchReceiver (\hits -> RecvSearch (List.map (Json.decodeValue entryDecoder) hits))


view : Model -> Html Msg
view model =
    layout [ padding 30 ] <|
        row [ spacing 7 ]
            [ column [ alignTop, spacing 7 ]
                [ text "Foo"
                , text "Bar"
                ]
            , column [ spacing 7 ]
                (Input.text
                    [ Input.focusedOnLoad ]
                    { onChange = Change
                    , text = model.query
                    , placeholder = Just <| Input.placeholder [] <| text "Type here"
                    , label = Input.labelAbove [] <| text "Text to search"
                    }
                    :: Input.checkbox []
                        { onChange = FuzzMode
                        , icon = Input.defaultCheckbox
                        , checked = model.isFuzzMode
                        , label = Input.labelRight [] <| text "Fuzzy search"
                        }
                    :: List.map viewHit model.hits
                )
            ]


viewHit : Result Json.Error Entry -> Element Msg
viewHit hit =
    hit
        |> Result.mapError (Json.errorToString >> text)
        |> Result.andThen
            (\entry ->
                Markdown.parse entry.hover.contents.value
                    |> Result.mapError (List.map (Markdown.deadEndToString >> text) >> column [])
                    |> Result.map (\markdown -> ( entry.projectId, markdown ))
            )
        |> Result.andThen
            (\( projectId, markdown ) ->
                Markdown.render elmUiRenderer markdown
                    |> Result.mapError text
                    |> Result.map (List.append [ text projectId ])
            )
        |> (\r ->
                case r of
                    Ok results ->
                        column [ Border.width 2, Border.rounded 6, Border.color <| rgb255 0xC0 0xC0 0xC0 ] <|
                            List.map (el [ paddingXY 10 0 ]) results

                    Err elem ->
                        elem
           )
