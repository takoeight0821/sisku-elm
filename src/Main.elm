port module Main exposing (..)

import Browser
import Dict as Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Lazy as Lazy
import Hovercraft exposing (Entry, entryDecoder)
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import Markdown
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
    , hits : RemoteData () (List (Result Json.Error Entry))
    }


type Msg
    = ChangeQuery String
    | FuzzMode Bool
    | ChangePlaceholder String
    | Contains String Bool
    | EnterWasPressed
    | RecvSearch String (List (Result Json.Error Entry))
    | RecvProjectIds (List String)


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
                    { model | query = newQuery }
            in
            ( newModel
            , Cmd.none
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

        EnterWasPressed ->
            let
                newModel =
                    { model | hits = Loading }
            in
            ( newModel
            , requestSearch (requestSearchParam newModel)
            )

        RecvSearch query hits ->
            if query == model.query then
                ( { model
                    | hits = Success hits
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
                    [ Input.focusedOnLoad, onEnter EnterWasPressed ]
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
                    column [ width fill ] <| List.map (Lazy.lazy viewHit) <| List.take 50 hits
            ]


viewHit : Result Json.Error Entry -> Element Msg
viewHit hit =
    hit
        |> Result.mapError (Json.errorToString >> text)
        |> Result.andThen
            (\entry ->
                Ok ( entry.projectId, entry.hover.contents.value )
            )
        |> Result.andThen
            (\( projectId, markdown ) ->
                Ok <| column [] [ html (Markdown.toHtml [] markdown), text projectId ]
            )
        |> (\r ->
                case r of
                    Ok result ->
                        column [ Border.width 2, Border.rounded 6, Border.color <| rgb255 0xC0 0xC0 0xC0 ]
                            [ el [ paddingXY 10 0 ] result ]

                    Err elem ->
                        elem
           )
