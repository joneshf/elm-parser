module Parser.String exposing (..)

import Parser exposing (..)

required : String -> Parser String (String -> b) -> Parser String b
required s =
  Parser.required (equal s)

nullable : String -> Parser String (Maybe String -> b) -> Parser String b
nullable s =
  Parser.nullable (equal s)

equal : a -> a -> Maybe a
equal x y =
  if x == y then
    Just x
  else
    Nothing
