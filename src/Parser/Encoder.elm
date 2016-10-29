module Parser.Encoder exposing (..)

import Json.Encode exposing (Value)
import Parser exposing (..)

type alias Encoder a =
  a -> Value

value : Encoder a -> Parser a Value
value encoder =
  Parser <| \a ->
    Ok (encoder a, a)

required : s -> Encoder a -> Parser a ((s, Value) -> b) -> Parser a b
required field encoder =
  Parser.required (\a -> Just (field, encoder a))

nullable : s -> Encoder a -> Parser a (Maybe (s, Value) -> b) -> Parser a b
nullable field encoder =
  Parser.nullable (\a -> Just (field, encoder a))
