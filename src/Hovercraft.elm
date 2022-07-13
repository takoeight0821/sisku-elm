module Hovercraft exposing (..)

import Json.Decode as Json exposing (Decoder, int, list, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias Hovercraft =
    { projectId : String, pages : List Page }


type alias Page =
    { entries : List Entry
    }


type alias TextDocumentIdentifier =
    { uri : String
    }


type alias Entry =
    { document : TextDocumentIdentifier
    , projectId : String
    , hover : Hover
    , definitions : List Definition
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

-- Decoders


hovercraftDecoder : Decoder Hovercraft
hovercraftDecoder =
    succeed Hovercraft
        |> required "projectId" string
        |> required "pages" (list pageDecoder)


pageDecoder : Decoder Page
pageDecoder =
    succeed Page
        |> required "entries" (list entryDecoder)


textDocumentIdentifierDecoder : Decoder TextDocumentIdentifier
textDocumentIdentifierDecoder =
    succeed TextDocumentIdentifier
        |> required "uri" string


entryDecoder : Decoder Entry
entryDecoder =
    succeed Entry
        |> required "document" textDocumentIdentifierDecoder
        |> required "projectId" string
        |> required "hover" hoverDecoder
        |> optional "definitions" (list definitionDecoder) []


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
