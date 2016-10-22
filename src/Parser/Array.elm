module Parser.Array exposing (..)

import Array exposing (Array)
import Parser exposing (..)

required : Int -> Parser (Array a) (a -> b) -> Parser (Array a) b
required n =
  Parser.required (Array.get n)

nullable : Int -> Parser (Array a) (Maybe a -> b) -> Parser (Array a) b
nullable n =
  Parser.nullable (Array.get n)
