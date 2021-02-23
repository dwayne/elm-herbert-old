module Test.Parser exposing (suite)


import Expect exposing (Expectation)
import Test exposing (..)


import H.AST exposing (..)
import H.Parser exposing (parse)


suite : Test
suite =
  describe "Parser"
    [ noProcedureDefinitions
    , procedureDefinitions
    , syntaxErrors
    ]


noProcedureDefinitions : Test
noProcedureDefinitions =
  describe "no procedure definitions"
    [ test "move" <|
        \_ ->
          equalH "s" <|
            H [] [S]
    , test "directions" <|
        \_ ->
          equalH "lr" <|
            H [] [L, R]
    , test "parameter" <|
        \_ ->
          equalH "A" <|
            H [] [SVar "A"]
    , test "procedure call" <|
        \_ ->
          equalH "a" <|
            H [] [Call "a" []]
    ]


procedureDefinitions : Test
procedureDefinitions =
  describe "procedure definitions"
    [ test "with no parameters" <|
        \_ ->
          equalH "a:ssra\na" <|
            H
              [Definition "a" [] [S, S, R, Call "a" []]]
              [Call "a" []]
    , test "with parameters" <|
        \_ ->
          equalH "f(A,B,C,D,E,F):sAf(AA,B,F-C+D+2,-D+1,E-1,sFl)\nf(sl,5,-1,100,,10,r)" <|
            H
              [ Definition
                  "f"
                  ["A", "B", "C", "D", "E", "F"]
                  [ S
                  , SVar "A"
                  , Call
                      "f"
                      [ Command [SVar "A", SVar "A"]
                      , Var "B"
                      , Formula <|
                          Expr
                            (Pos (TVar "F"))
                            [ Sub (TVar "C")
                            , Add (TVar "D")
                            , Add (Const 2)
                            ]
                      , Formula <|
                          Expr
                            (Neg (TVar "D"))
                            [ Add (Const 1)
                            ]
                      , Formula <|
                          Expr
                            (Pos (TVar "E"))
                            [ Sub (Const 1)
                            ]
                      , Command [S, SVar "F", L]
                      ]
                  ]
              ]
              [ Call
                  "f"
                  [ Command [S, L]
                  , Formula (Expr (Pos (Const 5)) [])
                  , Formula (Expr (Neg (Const 1)) [])
                  , Formula (Expr (Pos (Const 100)) [])
                  , Command []
                  , Formula (Expr (Pos (Const 10)) [])
                  , Command [R]
                  ]
              ]
    ]


syntaxErrors : Test
syntaxErrors =
  describe "syntax errors"
    -- NOTE:
    --
    -- These tests are brittle because they don't check the real cause for the
    -- error. However, they will do for now.
    [ test "missing procedure name" <|
        \_ ->
          parse ":ssra\na"
            |> Expect.err
    , test "missing procedure body" <|
        \_ ->
          parse "a:\na"
            |> Expect.err
    , test "missing main" <|
        \_ ->
          parse "a:ssra\n"
            |> Expect.err
    ]


-- HELPERS


equalH : String -> H -> Expectation
equalH input h =
  Expect.equal (Ok h) (parse input)
