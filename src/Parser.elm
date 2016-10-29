module Parser exposing (..)

type Parser s a
  = Parser (s -> Result ParseError ( a, s ))

type alias ParseError =
  List Error

type Error
  = Satisfy String
  | Fail String

run : Parser s a -> s -> Result ParseError (a, s)
run (Parser f) =
  f

with : s -> Parser s a -> Result ParseError (a, s)
with =
  flip run

parse : Parser s a -> s -> Result ParseError a
parse p =
  Result.map fst << run p

map : (a -> b) -> Parser s a -> Parser s b
map f (Parser g) =
  Parser <| Result.map (\( a, s ) -> ( f a, s )) << g

andThen : Parser s a -> (a -> Parser s b) -> Parser s b
andThen (Parser f) g =
  Parser <| flip Result.andThen (\( a, s ) -> with s (g a)) << f

succeed : a -> Parser s a
succeed a =
  Parser (\s -> Ok (a, s))

fail : String -> Parser s a
fail failure =
  Parser (always (Err [Fail failure]))

element : Parser (List a) a
element = Parser <| \s ->
  case s of
    [] -> Err [Satisfy "Expected at to parse at least one element. Given list was empty"]
    x::xs -> Ok (x, xs)

satisfy : (a -> Bool) -> Parser (List a) a
satisfy p =
  element `andThen` \a ->
  if p a then
    succeed a
  else
    fail ("Element " ++ toString a ++ " did not satisfy predicate")

required : (s -> Maybe a) -> Parser s (a -> b) -> Parser s b
required f parser =
  Parser <| \s ->
    case f s of
      Nothing ->
        Err [Fail ("Required constraint did not succeed for " ++ toString s)]
      Just a ->
        Result.map (\(g, s) -> (g a, s)) (run parser s)

nullable : (s -> Maybe a) -> Parser s (Maybe a -> b) -> Parser s b
nullable f parser =
  Parser <| \s ->
    Result.map (\(g, s) -> (g (f s), s)) (run parser s)

hardcoded : a -> Parser s (a -> b) -> Parser s b
hardcoded a =
  map (\f -> f a)
