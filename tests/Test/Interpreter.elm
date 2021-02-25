module Test.Interpreter exposing (suite)


import Expect
import Test exposing (..)


import H.Interpreter exposing (eval, Command(..), Error(..))
import Seq exposing (Seq)


suite : Test
suite =
  describe "Interpreter"
    [ examples
    , infiniteRecursion
    , runtimeErrors
    ]


examples : Test
examples =
  describe "examples"
    [ test "example 1" <|
        \_ ->
          run Nothing "sssssrsssssrsssssrsssssr"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 2" <|
        \_ ->
          run Nothing "a:sssssr\naaaa"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 3" <|
        \_ ->
          run (Just 24) "a:sssssra\na"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 4" <|
        \_ ->
          run Nothing "a(A):sssssra(A-1)\na(4)"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 5" <|
        \_ ->
          run Nothing "a(A,B):f(B)ra(A-1,B)\nf(A):sf(A-1)\na(4,5)"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 6" <|
        \_ ->
          run Nothing "a(A,B,C):f(B)Ca(A-1,B,C)\nf(A):sf(A-1)\na(4,5,r)"
            |> Expect.equal (Ok "sssssrsssssrsssssrsssssr")
    , test "example 7" <|
        \_ ->
          run (Just 17) "a(A):ArAa(AA)\na(s)"
            |> Expect.equal (Ok "srsssrssssssrssss")
    , test "example 8" <|
        \_ ->
          run (Just 100) "a(A,B,C):f(B)Ca(A-1,B,C)\nb(A):a(4,5,r)lb(A-1)\nf(A):sf(A-1)\nb(4)"
            |> Expect.equal (Ok (String.concat
                [ "sssssrsssssrsssssrsssssrl"
                , "sssssrsssssrsssssrsssssrl"
                , "sssssrsssssrsssssrsssssrl"
                , "sssssrsssssrsssssrsssssrl"
                ]
            ))
    , test "example 9" <|
        \_ ->
          run (Just 260) "a(A,B,C):f(B)Ca(A-1,B,C)\nb(A):a(4,A,r)b(A-1)\nf(A):sf(A-1)\nb(10)"
            |> Expect.equal (Ok (String.concat
                [ "ssssssssssrssssssssssrssssssssssrssssssssssr"  -- 40 + 4 = 44
                , "sssssssssrsssssssssrsssssssssrsssssssssr"      -- 40
                , "ssssssssrssssssssrssssssssrssssssssr"          -- 36
                , "sssssssrsssssssrsssssssrsssssssr"              -- 32
                , "ssssssrssssssrssssssrssssssr"                  -- 28
                , "sssssrsssssrsssssrsssssr"                      -- 24
                , "ssssrssssrssssrssssr"                          -- 20
                , "sssrsssrsssrsssr"                              -- 16
                , "ssrssrssrssr"                                  -- 12
                , "srsrsrsr"                                      -- 8
                ]
            ))
    , test "example 10" <|
        \_ ->
          run (Just 130) "a(A,B,C):f(B)Ca(A-1,B,C)\nb(A):a(2,11-A,r)b(A-1)\nf(A):sf(A-1)\nb(10)"
            |> Expect.equal (Ok (String.concat
                [ "srsr"                    -- 4
                , "ssrssr"                  -- 6
                , "sssrsssr"                -- 8
                , "ssssrssssr"              -- 10
                , "sssssrsssssr"            -- 12
                , "ssssssrssssssr"          -- 14
                , "sssssssrsssssssr"        -- 16
                , "ssssssssrssssssssr"      -- 18
                , "sssssssssrsssssssssr"    -- 20
                , "ssssssssssrssssssssssr"  -- 22
                ]
            ))
    , test "example 11" <|
        \_ ->
          run (Just 40) "a(A,B,C):f(B)Ca(A-1,B,C)\nf(A):sf(A-1)\na(4,5,rslsr)"
            |> Expect.equal (Ok "sssssrslsrsssssrslsrsssssrslsrsssssrslsr")
    ]



infiniteRecursion : Test
infiniteRecursion =
  -- FIXME: Make these examples work for 10,000 and more. Currently they will
  --        fail with "RangeError: Maximum call stack size exceeded".
  describe "infinite recursion"
    [ test "example 1" <|
        \_ ->
          run (Just 1000) "a:sa\na"
            |> Expect.ok
    , test "example 2" <|
        \_ ->
          run (Just 1000) "a(A):ArAa(AA)\na(s)"
            |> Expect.ok
    , test "example 3" <|
        \_ ->
          run (Just 1000) "a(A):Aa(AA)\na(s)"
            |> Expect.ok
    ]


runtimeErrors : Test
runtimeErrors =
  describe "runtime errors"
    [ test "procedure not found" <|
        \_ ->
          run Nothing "f"
            |> Expect.equal (Err (ProcedureNotFound "f"))
    , test "parameter not found" <|
        \_ ->
          run Nothing "a:Aa\na"
            |> Expect.equal (Err (ParameterNotFound "A"))
    , test "too few arguments" <|
        \_ ->
          run Nothing "a(A):Aa\na(s)"
            |> Expect.equal (Err (ArgumentError { expected = 1, given = 0 }))
    , test "too many arguments" <|
        \_ ->
          run Nothing "a(A,B):ABa(A,B,B)\na(s,r)"
            |> Expect.equal (Err (ArgumentError { expected = 2, given = 3 }))
    , test "expected a sequence" <|
        \_ ->
          run Nothing "a(A):Aa(A)\na(1)"
            |> Expect.equal (Err (TypeError "expected a sequence: 1"))
    , test "expected a number" <|
        \_ ->
          run Nothing "a(A):sa(A-1)\na(r)"
            |> Expect.equal (Err (TypeError "expected a number"))
    ]


-- HELPERS

run : Maybe Int -> String -> Result Error String
run steps =
  eval >> toString steps


toString : Maybe Int -> Seq Error Command -> Result Error String
toString steps seq =
  steps
    |> Maybe.map (\n -> Seq.take n seq)
    |> Maybe.withDefault seq
    |> Seq.toList
    |> Result.map (List.map commandToString >> String.concat)


commandToString : Command -> String
commandToString command =
  case command of
    Straight ->
      "s"

    Left ->
      "l"

    Right ->
      "r"
