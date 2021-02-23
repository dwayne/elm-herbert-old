module H.AST exposing
  ( H(..)
  , Definition(..)
  , Statement(..)
  , Expr(..), Term(..), SignedTerm(..), Factor(..)
  , Arg(..)
  , Name
  , Parameter
  )


type H
  = H (List Definition) (List Statement)


type Definition
  = Definition Name (List String) (List Statement)


type Statement
  = S
  | L
  | R
  | SVar Parameter
  | Call Name (List Arg)


type Expr
  = Expr SignedTerm (List Factor)


type Term
  = Const Int
  | TVar Parameter


type SignedTerm
  = Pos Term
  | Neg Term


type Factor
  = Add Term
  | Sub Term


type Arg
  = Var Parameter
  | Command (List Statement)
  | Formula Expr


type alias Name =
  String


type alias Parameter =
  String
