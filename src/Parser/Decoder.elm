module Parser.Decoder exposing (..)

import Json.Decode exposing (Decoder, Value, (:=))
import Parser exposing (..)

value : Decoder a -> Parser Value a
value decoder =
  Parser <| \s ->
    Json.Decode.decodeValue decoder s
      |> Result.map (flip (,) s)
      |> Result.formatError (\x -> [Fail x])

required : String -> Decoder a -> Parser Value (a -> b) -> Parser Value b
required field decoder =
  Parser.required (get field decoder)

nullable : String -> Decoder a -> Parser Value (Maybe a -> b) -> Parser Value b
nullable field decoder =
  Parser.nullable (get field decoder)

get : String -> Decoder a -> Value -> Maybe a
get field decoder value =
  case Json.Decode.decodeValue (field := decoder) value of
    Err _ ->
      Nothing
    Ok a ->
      Just a
