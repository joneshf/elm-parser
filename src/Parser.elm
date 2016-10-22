module Parser exposing (..)

type Parser s a
    = Parser (s -> List ( a, s ))

run : Parser s a -> s -> List ( a, s )
run (Parser f) =
  f

with : s -> Parser s a -> List ( a, s )
with s (Parser f) =
  f s


map : (a -> b) -> Parser s a -> Parser s b
map f (Parser g) =
  Parser <| List.map (\( a, s ) -> ( f a, s )) << g


andThen : Parser s a -> (a -> Parser s b) -> Parser s b
andThen (Parser f) g =
  Parser <| List.concatMap (\( a, s ) -> with s (g a)) << f


succeed : a -> Parser s a
succeed a =
  Parser <| \s -> [ ( a, s ) ]


fail : Parser s a
fail =
  Parser <| always []

element : Parser (List a) a
element = Parser <| \s ->
  case s of
    [] -> []
    x::xs -> [(x, xs)]

satisfy : (a -> Bool) -> Parser (List a) a
satisfy p =
  element `andThen` \a ->
  if p a then
    succeed a
  else
    fail

required : (s -> Maybe a) -> Parser s (a -> b) -> Parser s b
required f parser =
  Parser <| \s ->
    case f s of
      Nothing ->
        []
      Just a ->
        List.map (\(g, s) -> (g a, s)) (run parser s)

nullable : (s -> Maybe a) -> Parser s (Maybe a -> b) -> Parser s b
nullable f parser =
  Parser <| \s ->
    List.map (\(g, s) -> (g (f s), s)) (run parser s)

hardcoded : a -> Parser s (a -> b) -> Parser s b
hardcoded a =
  map (\f -> f a)
