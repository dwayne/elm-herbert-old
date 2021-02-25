module Test.Env exposing (suite)


import Expect exposing (Expectation)
import Test exposing (..)


import Env exposing (Env)


suite : Test
suite =
  describe "Env"
    [ lookup
    ]


lookup : Test
lookup =
  let
    env : Env String Int
    env =
      Env.empty
        |> Env.extend "x" 1
        |> Env.extend "y" 2
        |> Env.extend "z" 3
  in
  describe "lookup"
    [ test "when name exists" <|
        \_ ->
          Env.lookup "y" env
            |> Expect.equal (Just 2)
    , test "when name does not exist" <|
        \_ ->
          Env.lookup "a" env
            |> Expect.equal Nothing
    , test "when name is shadowed" <|
        \_ ->
          Env.extend "y" 4 env
            |> Env.lookup "y"
            |> Expect.equal (Just 4)
    ]
