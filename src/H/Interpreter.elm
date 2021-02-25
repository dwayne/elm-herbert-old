module H.Interpreter exposing (eval, Command(..), Error(..))


import Env
import H.AST exposing (..)
import H.Parser exposing (parse)
import Parser exposing (DeadEnd)
import Seq exposing (Seq)


type alias Environment = Env.Env Parameter DenotedValue


type DenotedValue
  = NumVal Int
  | SeqVal (Seq Error Command)


type Command
  = Straight
  | Left
  | Right


type Error
  = ArgumentError { expected: Int, given: Int }
  | NotImplemented String
  | ParameterNotFound Parameter
  | ParseError (List DeadEnd)
  | ProcedureNotFound String
  | StopIteration
  | TypeError String


eval : String -> Seq Error Command
eval input =
  case parse input of
    Ok (H defs stmts) ->
      evalStatements defs stmts Env.empty

    Err deadEnds ->
      Seq.error (ParseError deadEnds)


evalStatements : List Definition -> List Statement -> Environment -> Seq Error Command
evalStatements defs stmts env =
  case stmts of
    [] ->
      Seq.empty

    (stmt :: rest) ->
      Seq.append
        (evalStatement defs stmt env)
        (\_ -> evalStatements defs rest env)


evalStatement : List Definition -> Statement -> Environment -> Seq Error Command
evalStatement defs stmt env =
  case stmt of
    S ->
      Seq.singleton Straight

    L ->
      Seq.singleton Left

    R ->
      Seq.singleton Right

    SVar param ->
      case Env.lookup param env of
        Just value ->
          case value of
            SeqVal seq ->
              seq

            NumVal n ->
              Seq.error (TypeError ("expected a sequence: " ++ String.fromInt n))

        Nothing ->
          Seq.error (ParameterNotFound param)

    Call name args ->
      case lookupDefinition name defs of
        Just def ->
          evalDefinition defs def args env

        Nothing ->
          Seq.error (ProcedureNotFound name)


evalDefinition : List Definition -> Definition -> List Arg -> Environment -> Seq Error Command
evalDefinition defs (Definition _ params body) args env =
  case evalArguments defs params args env of
    Ok nextEnv ->
      evalStatements defs body nextEnv

    Err StopIteration ->
      Seq.empty

    Err e ->
      Seq.error e


evalArguments : List Definition -> List Parameter -> List Arg -> Environment -> Result Error Environment
evalArguments defs params args env =
  let
    paramsLength =
      List.length params

    argsLength =
      List.length args
  in
  if argsLength == paramsLength then
    evalArgumentsHelper defs params args env env
  else
    Err (ArgumentError { expected = paramsLength, given = argsLength })


evalArgumentsHelper : List Definition -> List Parameter -> List Arg -> Environment -> Environment -> Result Error Environment
evalArgumentsHelper defs params args env nextEnv =
  case (params, args) of
    (param :: restParams, arg :: restArgs) ->
      evalArgument defs arg env
        |> Result.andThen (
            \value ->
              case value of
                NumVal n ->
                  if n == 0 then
                    Err StopIteration
                  else
                    Ok value

                _ ->
                  Ok value
        )
        |> Result.map (\value -> Env.extend param value nextEnv)
        |> Result.andThen (evalArgumentsHelper defs restParams restArgs env)

    _ ->
      Ok nextEnv


evalArgument : List Definition -> Arg -> Environment -> Result Error DenotedValue
evalArgument defs arg env =
  case arg of
    Var param ->
      Env.lookup param env
        |> Result.fromMaybe (ParameterNotFound param)

    Command stmts ->
      Ok <| SeqVal (evalStatements defs stmts env)

    Formula expr ->
      evalExpression expr env
        |> Result.map NumVal


evalExpression : Expr -> Environment -> Result Error Int
evalExpression (Expr signedTerm factors) env =
  evalSignedTerm signedTerm env
    |> Result.andThen (\accumulator -> sumExpression accumulator factors env)


evalSignedTerm : SignedTerm -> Environment -> Result Error Int
evalSignedTerm signedTerm env =
  case signedTerm of
    Pos term ->
      evalTerm term env

    Neg term ->
      evalTerm term env
        |> Result.map negate


evalTerm : Term -> Environment -> Result Error Int
evalTerm term env =
  case term of
    Const n ->
      Ok n

    TVar param ->
      case Env.lookup param env of
        Just value ->
          case value of
            NumVal n ->
              Ok n

            SeqVal seq ->
              Err (TypeError "expected a number")

        Nothing ->
          Err (ParameterNotFound param)


sumExpression : Int -> List Factor -> Environment -> Result Error Int
sumExpression accumulator factors env =
  case factors of
    [] ->
      Ok accumulator

    (factor :: restFactors) ->
      case factor of
        Add term ->
          evalTerm term env
            |> Result.andThen (\n -> sumExpression (accumulator + n) restFactors env)

        Sub term ->
          evalTerm term env
            |> Result.andThen (\n -> sumExpression (accumulator - n) restFactors env)


-- HELPERS


lookupDefinition : String -> List Definition -> Maybe Definition
lookupDefinition searchName defs =
  case defs of
    [] ->
      Nothing

    (((Definition name _ _) as def) :: rest) ->
      if name == searchName then
        Just def
      else
        lookupDefinition searchName rest
