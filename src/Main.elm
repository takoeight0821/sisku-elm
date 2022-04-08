port module Main exposing (..)

import Browser
import Dict as Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Hovercraft exposing (Entry, entryDecoder)
import Html exposing (Html)
import Html.Events
import Json.Decode as Json exposing (float, succeed)
import Json.Decode.Pipeline exposing (required)
import Markdown
import Paginate exposing (PaginatedList)
import RemoteData exposing (RemoteData(..))


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


port requestSearch :
    { placeholder : String
    , isFuzzMode : Bool
    , projectIds : List String
    , query : String
    }
    -> Cmd msg


port searchReceiver : (( String, List Json.Value ) -> msg) -> Sub msg


port projectIdsReceiver : (List String -> msg) -> Sub msg


type alias Model =
    { projectIds : Dict String Bool
    , query : String
    , isFuzzMode : Bool
    , placeholder : String
    , hits : RemoteData () (PaginatedList (Result Json.Error ( Entry, Float )))
    }


type Msg
    = ChangeQuery String
    | FuzzMode Bool
    | ChangePlaceholder String
    | Contains String Bool
    | RecvSearch String (List (Result Json.Error ( Entry, Float )))
    | RecvProjectIds (List String)
      -- for pagination
    | First
    | Prev
    | Next
    | Last
    | GoTo Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { projectIds = Dict.fromList []
      , query = ""
      , isFuzzMode = False
      , placeholder = "_"
      , hits = NotAsked
      }
    , Cmd.none
    )


requestSearchParam : Model -> { placeholder : String, isFuzzMode : Bool, projectIds : List String, query : String }
requestSearchParam model =
    { placeholder = model.placeholder
    , isFuzzMode = model.isFuzzMode
    , projectIds =
        List.concatMap
            (\( projectId, enabled ) ->
                if enabled then
                    [ projectId ]

                else
                    []
            )
            (Dict.toList model.projectIds)
    , query = model.query
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeQuery newQuery ->
            let
                newModel =
                    { model
                        | query = newQuery
                        , hits =
                            case model.hits of
                                NotAsked ->
                                    Loading

                                _ ->
                                    model.hits
                    }
            in
            ( newModel
            , requestSearch (requestSearchParam newModel)
            )

        FuzzMode flag ->
            let
                newModel =
                    { model | isFuzzMode = flag, hits = Loading }
            in
            ( newModel
            , requestSearch (requestSearchParam newModel)
            )

        ChangePlaceholder newPlaceholder ->
            let
                newModel =
                    { model | placeholder = newPlaceholder, hits = Loading }
            in
            ( newModel
            , requestSearch (requestSearchParam newModel)
            )

        Contains projectId flag ->
            let
                newModel =
                    { model | projectIds = Dict.insert projectId flag model.projectIds, hits = Loading }
            in
            ( newModel
            , requestSearch (requestSearchParam newModel)
            )

        RecvSearch query hits ->
            if query == model.query then
                ( { model
                    | hits = Success (Paginate.fromList 10 hits)
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

        First ->
            ( { model | hits = RemoteData.map Paginate.first model.hits }, Cmd.none )

        Prev ->
            ( { model | hits = RemoteData.map Paginate.prev model.hits }, Cmd.none )

        Next ->
            ( { model | hits = RemoteData.map Paginate.next model.hits }, Cmd.none )

        Last ->
            ( { model | hits = RemoteData.map Paginate.last model.hits }, Cmd.none )

        GoTo index ->
            ( { model | hits = RemoteData.map (Paginate.goTo index) model.hits }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ searchReceiver
            (\( query, hits ) ->
                RecvSearch query
                    (List.map
                        (Json.decodeValue
                            (succeed Tuple.pair
                                |> required "hit" entryDecoder
                                |> required "score" float
                            )
                        )
                     <|
                        List.take 50 hits
                    )
            )
        , projectIdsReceiver RecvProjectIds
        ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.field "key" Json.string
                |> Json.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.succeed msg

                        else
                            Json.fail "Not the enter key"
                    )
            )
        )


view : Model -> Html Msg
view model =
    let
        projectIdList =
            List.map
                (\projectId ->
                    Input.checkbox []
                        { onChange = Contains projectId
                        , icon = Input.defaultCheckbox
                        , checked = Maybe.withDefault False (Dict.get projectId model.projectIds)
                        , label = Input.labelRight [] <| text projectId
                        }
                )
                (Dict.keys model.projectIds)

        searchForm =
            column [ width fill ]
                [ Input.text
                    [ Input.focusedOnLoad ]
                    { onChange = ChangeQuery
                    , text = model.query
                    , placeholder = Just <| Input.placeholder [] <| text "Type here and press enter"
                    , label = Input.labelLeft [] <| text "Text to search"
                    }
                , Input.text
                    []
                    { onChange = ChangePlaceholder
                    , text = model.placeholder
                    , placeholder = Just <| Input.placeholder [] <| text "_"
                    , label = Input.labelLeft [] <| text "Placeholder"
                    }
                , Input.checkbox []
                    { onChange = FuzzMode
                    , icon = Input.defaultCheckbox
                    , checked = model.isFuzzMode
                    , label = Input.labelRight [] <| text "Fuzzy search"
                    }
                ]

        prevButtons =
            [ Input.button [ Border.width 2 ] { onPress = Just First, label = text "<<" }
            , Input.button [ Border.width 2 ] { onPress = Just Prev, label = text "<" }
            ]

        nextButtons =
            [ Input.button [ Border.width 2 ] { onPress = Just Next, label = text ">" }
            , Input.button [ Border.width 2 ] { onPress = Just Last, label = text ">>" }
            ]

        pagerButtonView index isActive =
            Input.button
                [ if isActive then
                    Font.bold

                  else
                    Font.regular
                , Border.width 2
                ]
                { onPress = Just <| GoTo index
                , label = text <| String.fromInt index
                }
    in
    layout [ padding 30 ] <|
        column [ spacing 7, width fill ]
            [ column [] projectIdList
            , searchForm
            , case model.hits of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading..."

                Failure _ ->
                    text "ERROR"

                Success hits ->
                    column [ width fill ] <|
                        List.map (Lazy.lazy viewHit) (Paginate.page hits)
                            ++ [ row [ spacing 2 ] <| prevButtons ++ Paginate.pager pagerButtonView hits ++ nextButtons ]
            ]


viewHit : Result Json.Error ( Entry, Float ) -> Element Msg
viewHit hit =
    hit
        |> Result.mapError (Json.errorToString >> text)
        |> Result.andThen
            (\( entry, score ) ->
                Ok ( entry.projectId, entry.hover.contents.value, score )
            )
        |> Result.andThen
            (\( projectId, markdown, score ) ->
                Ok <| column [] [ html (Markdown.toHtml [] markdown), text projectId, text ("Distance: " ++ String.fromFloat score) ]
            )
        |> (\r ->
                case r of
                    Ok result ->
                        column [ Border.width 2, Border.rounded 6, Border.color <| rgb255 0xC0 0xC0 0xC0 ]
                            [ el [ paddingXY 10 0 ] result ]

                    Err elem ->
                        elem
           )
