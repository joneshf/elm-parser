module Parser.Dict exposing (..)

import Dict exposing (Dict)
import Parser exposing (..)

required : comparable -> Parser (Dict comparable a) (a -> b) -> Parser (Dict comparable a) b
required key =
  Parser.required (Dict.get key)

nullable : comparable -> Parser (Dict comparable a) (Maybe a -> b) -> Parser (Dict comparable a) b
nullable key =
  Parser.nullable (Dict.get key)

get : comparable -> Parser (Dict comparable a) a
get key =
  Parser <| \dict ->
    case Dict.get key dict of
      Nothing ->
        []
      Just a ->
        [(a, dict)]
