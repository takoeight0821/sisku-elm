module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Json.Decode as Json exposing (Decoder, int, list, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "" }


type Msg
    = Change String



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
        |> required "definitions" (list definitionDecoder)
        |> required "moniker" (list monikerDecoder)


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to search", value model.content, onInput Change ] []
        , div [] [ text model.content ]
        ]


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
exampleJsonMultiple = """
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