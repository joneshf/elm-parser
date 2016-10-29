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

parse :
  (String -> number -> Maybe String -> Maybe String -> String -> a)
  -> (String -> s -> Maybe String)
  -> Parser s a
parse user getter =
  succeed user
    |> required (getter "doActivityUrl")
    |> hardcoded 37
    |> nullable (getter "viewSummaryUrl")
    |> nullable (getter "studentAnswersUrl")
    |> hardcoded "/details"

fromDict : Parser (Dict String String) User
fromDict =
  parse User Dict.get

fromValue : Parser Value User
fromValue =
  parse User (flip Parser.Decoder.get Decode.string)

encode = \a b c d e ->
  [ Just (Encode.string a)
  , Just (Encode.int b)
  , Maybe.map Encode.string c
  , Maybe.map Encode.string d
  , Just (Encode.string e)
  ]

toValue : Parser User (List (Maybe Value))
toValue =
  parse encode <| \str {doActivityUrl, viewSummaryUrl, studentAnswersUrl} ->
    case str of
      "doActivityUrl" ->
        Just doActivityUrl
      "viewSummaryUrl" ->
        viewSummaryUrl
      "studentAnswersUrl" ->
        studentAnswersUrl
      _ ->
        Nothing

urls : List (String, String)
urls =
  [ ("doActivityUrl", "foo")
  , ("viewDetailsUrl", "bar")
  , ("studentAnswersUrl", "baz")
  ]

withDictParser : Parser (Dict String String) a -> Result ParseError a
withDictParser parser =
  Parser.parse parser (Dict.fromList urls)

withValueParser : Parser Value a -> Result ParseError a
withValueParser parser =
  Parser.parse parser (Encode.object (List.map (\(x, y) -> (x, Encode.string y)) urls))
