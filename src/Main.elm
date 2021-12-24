module Main exposing (..)

import Browser
import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Json exposing (Decoder, int, list, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { query : String, hits : List Hit }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { query = "", hits = [] }, Cmd.none )


fetchSearchResults : String -> Cmd Msg
fetchSearchResults query =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:9200/hovercraft/_search"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "size", E.float 10000 )
                    , ( "query"
                      , E.object
                            [ ( "match"
                              , E.object
                                    [ ( "hover.contents.value"
                                      , E.object [ ( "query", E.string (Debug.log "query" query) ), ( "fuzziness", E.string "AUTO" ) ]
                                      )
                                    ]
                              )
                            ]
                      )
                    ]
                )
        , expect = Http.expectJson Hits hitsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type Msg
    = Change String
    | Hits (Result Http.Error (List Hit))



-- | Hit data from ElasticSearch.


type alias Hit =
    { source : Hovercraft }


type alias Hovercraft =
    { hover : Hover
    , definitions : List Definition
    , moniker : List Moniker
    }


type alias Hover =
    { contents : MarkupContent
    , range : Maybe Range
    }


type alias MarkupContent =
    { kind : String
    , value : String
    }


type alias Range =
    { start : Position
    , end : Position
    }


type alias Position =
    { line : Int
    , character : Int
    }


type Definition
    = Location
        { uri : String
        , range : Range
        }
    | LocationLink
        { originSelectionRange : Maybe Range
        , targetUri : String
        , targetRange : Range
        , targetSelectionRange : Range
        }


type alias Moniker =
    { scheme : String
    , identifier : String
    , unique : String
    , kind : Maybe String
    }



-- | The decoder for the JSON response from ElasticSearch.


hitsDecoder : Decoder (List Hit)
hitsDecoder =
    succeed identity
        |> required "hits" (succeed identity |> required "hits" (list hitDecoder))


hitDecoder : Decoder Hit
hitDecoder =
    succeed Hit
        |> required "_source" hovercraftDecoder


hovercraftDecoder : Decoder Hovercraft
hovercraftDecoder =
    succeed Hovercraft
        |> required "hover" hoverDecoder
        |> optional "definitions" (list definitionDecoder) []
        |> optional "moniker" (list monikerDecoder) []


hoverDecoder : Decoder Hover
hoverDecoder =
    succeed Hover
        |> required "contents" markupContentDecoder
        |> optional "range" (Json.map Just rangeDecoder) Nothing


markupContentDecoder : Decoder MarkupContent
markupContentDecoder =
    succeed MarkupContent
        |> required "kind" string
        |> required "value" string


rangeDecoder : Decoder Range
rangeDecoder =
    succeed Range
        |> required "start" positionDecoder
        |> required "end" positionDecoder


positionDecoder : Decoder Position
positionDecoder =
    succeed Position
        |> required "line" int
        |> required "character" int


definitionDecoder : Decoder Definition
definitionDecoder =
    oneOf [ locationDecoder, locationLinkDecoder ]


locationDecoder : Decoder Definition
locationDecoder =
    succeed (\uri range -> Location { uri = uri, range = range })
        |> required "uri" string
        |> required "range" rangeDecoder


locationLinkDecoder : Decoder Definition
locationLinkDecoder =
    succeed (\originSelectionRange targetUri targetRange targetSelectionRange -> LocationLink { originSelectionRange = originSelectionRange, targetUri = targetUri, targetRange = targetRange, targetSelectionRange = targetSelectionRange })
        |> optional "originSelectionRange" (Json.map Just rangeDecoder) Nothing
        |> required "targetUri" string
        |> required "targetRange" rangeDecoder
        |> required "targetSelectionRange" rangeDecoder


monikerDecoder : Decoder Moniker
monikerDecoder =
    succeed Moniker
        |> required "scheme" string
        |> required "identifier" string
        |> required "unique" string
        |> optional "kind" (Json.map Just string) Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newQuery ->
            ( { model
                | query = newQuery
              }
            , fetchSearchResults newQuery
            )

        Hits (Ok hits) ->
            ( { model
                | hits = hits
              }
            , Cmd.none
            )

        Hits (Err err) ->
            let
                _ =
                    Debug.log "Err" (Debug.toString err)
            in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to search", value model.query, onInput Change ] []
        , div []
            [ text
                (if String.isEmpty model.query then
                    "Please enter a search term"

                 else
                    "Searching for " ++ model.query
                )
            ]
        , div []
            [ if List.isEmpty model.hits then
                text "No results"

              else
                ul [] (List.map viewHit model.hits)
            ]
        ]


viewHit : Hit -> Html Msg
viewHit hit =
    li [] [ text hit.source.hover.contents.value ]


exampleJson : String
exampleJson =
    """
{
    "hits": {
        "total": {
            "value": 1,
            "relation": "eq"
        },
        "hits": [
            {
                "_index": "test",
                "_type": "test",
                "_id": "1",
                "_score": 1,
                "_source": {
                    "hover": {
                        "contents": {
                            "kind": "markdown",
                            "value": "This is a test"
                        },
                        "range": {
                            "start": {
                                "line": 0,
                                "character": 0
                            },
                            "end": {
                                "line": 0,
                                "character": 0
                            }
                        }
                    },
                    "definitions": [
                        {
                            "uri": "file:///Users/james/Projects/elm-lang/elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "range": {
                                "start": {
                                    "line": 0,
                                    "character": 0
                                },
                                "end": {
                                    "line": 0,
                                    "character": 0
                                }
                            }
                        }
                    ],
                    "moniker": [
                        {
                            "scheme": "elm",
                            "identifier": "Compiler.Pipeline",
                            "unique": "elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "kind": "module"
                        }
                    ] 
                }
            }
        ]
    }
}
"""



-- | The test data that has 3 hits.


exampleJsonMultiple : String
exampleJsonMultiple =
    """
{
    "hits": {
        "total": {
            "value": 3,
            "relation": "eq"
        },
        "hits": [
            {
                "_index": "test",
                "_type": "test",
                "_id": "1",
                "_score": 1,
                "_source": {
                    "hover": {
                        "contents": {
                            "kind": "markdown",
                            "value": "This is a test"
                        },
                        "range": {
                            "start": {
                                "line": 0,
                                "character": 0
                            },
                            "end": {
                                "line": 0,
                                "character": 0
                            }
                        }
                    },
                    "definitions": [
                        {
                            "uri": "file:///Users/james/Projects/elm-lang/elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "range": {
                                "start": {
                                    "line": 0,
                                    "character": 0
                                },
                                "end": {
                                    "line": 0,
                                    "character": 0
                                }
                            }
                        }
                    ],
                    "moniker": [
                        {
                            "scheme": "elm",
                            "identifier": "Compiler.Pipeline",
                            "unique": "elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "kind": "module"
                        }
                    ] 
                }
            },
            {
                "_index": "test",
                "_type": "test",
                "_id": "2",
                "_score": 1,
                "_source": {
                    "hover": {
                        "contents": {
                            "kind": "markdown",
                            "value": "This is a test"
                        },
                        "range": {
                            "start": {
                                "line": 0,
                                "character": 0
                            },
                            "end": {
                                "line": 0,
                                "character": 0
                            }
                        }
                    },
                    "definitions": [
                        {
                            "uri": "file:///Users/james/Projects/elm-lang/elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "range": {
                                "start": {
                                    "line": 0,
                                    "character": 0
                                },
                                "end": {
                                    "line": 0,
                                    "character": 0
                                }
                            }
                        }
                    ],
                    "moniker": [
                        {
                            "scheme": "elm",
                            "identifier": "Compiler.Pipeline",
                            "unique": "elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "kind": "module"
                        }
                    ]
                }
            },
            {
                "_index": "test",
                "_type": "test",
                "_id": "3",
                "_score": 1,
                "_source": {
                    "hover": {
                        "contents": {
                            "kind": "markdown",
                            "value": "This is a test"
                        },
                        "range": {
                            "start": {
                                "line": 0,
                                "character": 0
                            },
                            "end": {
                                "line": 0,
                                "character": 0
                            }
                        }
                    },
                    "definitions": [
                        {
                            "uri": "file:///Users/james/Projects/elm-lang/elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "range": {
                                "start": {
                                    "line": 0,
                                    "character": 0
                                },
                                "end": {
                                    "line": 0,
                                    "character": 0
                                }
                            }
                        }
                    ],
                    "moniker": [
                        {
                            "scheme": "elm",
                            "identifier": "Compiler.Pipeline",
                            "unique": "elm-compiler/src/Elm/Compiler/Pipeline.elm",
                            "kind": "module"
                        }
                    ]
                }
            }
        ]
    }
}
"""
