module User exposing (..)

import Dict exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Parser exposing (..)
import Parser.Decoder

type alias User =
  { doActivityUrl : String
  , arbitrary : Int
  , viewSummaryUrl : Maybe String
  , studentAnswersUrl : Maybe String
  , viewDetailsUrl : String
  }

parse : (String -> s -> Maybe String) -> Parser s User
parse getter =
  succeed User
    |> required (getter "doActivityUrl")
    |> hardcoded 37
    |> nullable (getter "viewSummaryUrl")
    |> nullable (getter "studentAnswersUrl")
    |> hardcoded "/details"

fromDict : Parser (Dict String String) User
fromDict =
  parse Dict.get

fromValue : Parser Value User
fromValue =
  parse (flip Parser.Decoder.get Decode.string)

urls : List (String, String)
urls =
  [ ("doActivityUrl", "foo")
  , ("viewDetailsUrl", "bar")
  , ("studentAnswersUrl", "baz")
  ]

withDictParser : Parser (Dict String String) a -> List a
withDictParser parser =
  parser
    |> with (Dict.fromList urls)
    |> List.map fst

withValueParser : Parser Value a -> List a
withValueParser parser =
  parser
    |> with (Encode.object (List.map (\(x, y) -> (x, Encode.string y)) urls))
    |> List.map fst
