module H.Parser exposing (parse)


import H.AST exposing (..)
import Parser exposing (..)


parse : String -> Result (List DeadEnd) H
parse =
  run h


-- PARSERS


h : Parser H
h =
  let
    program =
      loop ([], []) programHelper

    programHelper (revDefinitions, body) =
      oneOf
        [ succeed (\d -> Loop (d :: revDefinitions, body))
            |= backtrackable definition
        , succeed (\b -> Done (List.reverse revDefinitions, b))
            |= many1 statement
            |. spaces
            |. end
        ]
  in
  map (\(definitions, body) -> H definitions body) program


definition : Parser Definition
definition =
  let
    parameterList =
      sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = succeed ()
        , item = parameter
        , trailing = Forbidden
        }
  in
  succeed Definition
    |= name
    |= optional [] parameterList
    |. symbol ":"
    |= many1 statement
    |. symbol "\n"


statement : Parser Statement
statement =
  let
    argList =
      sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = succeed ()
        , item = arg
        , trailing = Forbidden
        }
  in
  oneOf
    [ succeed S
        |. symbol "s"
    , succeed L
        |. symbol "l"
    , succeed R
        |. symbol "r"
    , succeed SVar
        |= parameter
    , succeed Call
        |= name
        |= optional [] argList
    ]


expr : Parser Expr
expr =
  let
    signedTerm =
      oneOf
        [ succeed Neg
            |. symbol "-"
            |= term
        , succeed Pos
            |= term
        ]
  in
  succeed Expr
    |= signedTerm
    |= many factor


factor : Parser Factor
factor =
  oneOf
    [ succeed Add
        |. symbol "+"
        |= term
    , succeed Sub
        |. symbol "-"
        |= term
    ]


term : Parser Term
term =
  oneOf
    [ map Const number
    , map TVar parameter
    ]


arg : Parser Arg
arg =
  let
    parameterFollowedByOneOrMoreStmts =
      succeed (\p stmts -> Command (SVar p :: stmts))
          |= parameter
          |= lazy (\_ -> many1 statement)

    parameterFollowedByOneOrMoreFactors =
      succeed (\p factors -> Formula (Expr (Pos (TVar p)) factors))
          |= parameter
          |= many1 factor
  in
  oneOf
    [ backtrackable parameterFollowedByOneOrMoreStmts
    , backtrackable parameterFollowedByOneOrMoreFactors
    , succeed Var
        |= parameter
    , succeed Formula
        |= expr
    , succeed Command
        |= lazy (\_ -> many statement)
    ]


-- PARSER HELPERS


optional : a -> Parser a -> Parser a
optional default p =
  oneOf
    [ p
    , succeed default
    ]


many : Parser a -> Parser (List a)
many p =
  let
    helper rev =
      oneOf
        [ succeed (\x -> Loop (x :: rev))
            |= p
        , succeed ()
            |> map (\_ -> Done (List.reverse rev))
        ]
  in
  loop [] helper


many1 : Parser a -> Parser (List a)
many1 p =
  succeed (::)
    |= p
    |= many p


-- LEXERS


number : Parser Int
number =
  let
    checkRange n =
      if n >= -256 && n <= 255 then
        succeed n
      else
        problem "a number must be in the range -256 to 255 inclusive"
  in
  int |> andThen checkRange


name : Parser String
name =
  char isName


parameter : Parser String
parameter =
  char isParameter


-- LEXER HELPERS


char : (Char -> Bool) -> Parser String
char pred =
  getChompedString (chompIf pred)


isName : Char -> Bool
isName =
  isOneOf "abcdefghijkmnopqtuvwxyz"


isParameter : Char -> Bool
isParameter =
  isOneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


isOneOf : String -> Char -> Bool
isOneOf alternatives c =
  alternatives
    |> String.toList
    |> List.any ((==) c)
