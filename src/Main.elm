port module Main exposing (..)

import Browser
import Dict as Dict exposing (Dict)
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


port requestSearch : ( String, Bool, String ) -> Cmd msg


port searchReceiver : (( String, List Json.Value ) -> msg) -> Sub msg


port projectIdsReceiver : (List String -> msg) -> Sub msg


type alias Model =
    { projectIds : Dict String Bool
    , query : String
    , isFuzzMode : Bool
    , placeholder : String
    , hits : List (Result Json.Error Entry)
    }


type Msg
    = ChangeQuery String
    | FuzzMode Bool
    | ChangePlaceholder String
    | Contains String Bool
    | RecvSearch String (List (Result Json.Error Entry))
    | RecvProjectIds (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { projectIds = Dict.fromList []
      , query = ""
      , isFuzzMode = False
      , placeholder = "_"
      , hits = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeQuery newQuery ->
            ( { model
                | query = newQuery
              }
            , requestSearch ( model.placeholder, model.isFuzzMode, newQuery )
            )

        FuzzMode flag ->
            ( { model
                | isFuzzMode = flag
              }
            , requestSearch ( model.placeholder, flag, model.query )
            )

        ChangePlaceholder newPlaceholder ->
            ( { model
                | placeholder = newPlaceholder
              }
            , requestSearch ( newPlaceholder, model.isFuzzMode, model.query )
            )

        Contains projectId flag ->
            ( { model | projectIds = Dict.insert projectId flag model.projectIds }, Cmd.none )

        RecvSearch query hits ->
            if query == model.query then
                ( { model
                    | hits = hits
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RecvProjectIds projectIds ->
            ( { model
                | projectIds = Dict.fromList (List.map (\id -> ( id, True )) projectIds)
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ searchReceiver (\( query, hits ) -> RecvSearch query (List.map (Json.decodeValue entryDecoder) hits))
        , projectIdsReceiver RecvProjectIds
        ]


view : Model -> Html Msg
view model =
    layout [ padding 30 ] <|
        column [ spacing 7 ]
            [ column [ alignTop, spacing 7 ]
                (List.map
                    (\projectId ->
                        Input.checkbox []
                            { onChange = Contains projectId
                            , icon = Input.defaultCheckbox
                            , checked = Maybe.withDefault False (Dict.get projectId model.projectIds)
                            , label = Input.labelRight [] <| text projectId
                            }
                    )
                    (Dict.keys model.projectIds)
                )
            , column [ spacing 7 ]
                (row [ spacing 7 ]
                    [ Input.text
                        [ Input.focusedOnLoad ]
                        { onChange = ChangeQuery
                        , text = model.query
                        , placeholder = Just <| Input.placeholder [] <| text "Type here"
                        , label = Input.labelAbove [] <| text "Text to search"
                        }
                    , Input.text
                        []
                        { onChange = ChangePlaceholder
                        , text = model.placeholder
                        , placeholder = Just <| Input.placeholder [] <| text "_"
                        , label = Input.labelAbove [] <| text "Placeholder"
                        }
                    ]
                    :: Input.checkbox []
                        { onChange = FuzzMode
                        , icon = Input.defaultCheckbox
                        , checked = model.isFuzzMode
                        , label = Input.labelRight [] <| text "Fuzzy search"
                        }
                    :: List.map viewHit
                        (model.hits
                            |> List.filter
                                (\r ->
                                    case r of
                                        Err _ ->
                                            True

                                        Ok entry ->
                                            case Dict.get entry.projectId model.projectIds of
                                                Nothing ->
                                                    False

                                                Just flag ->
                                                    flag
                                )
                        )
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
