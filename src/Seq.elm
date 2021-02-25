module Seq exposing
  ( Seq
  , empty, singleton, append, error
  , head, tail, take
  , toList
  )


type Seq e a
  = Error e
  | Nil
  | Cons a (() -> Seq e a)
  | Append (Seq e a) (() -> Seq e a)


-- CONSTRUCTORS


empty : Seq e a
empty =
  Nil


singleton : a -> Seq e a
singleton a =
  Cons a (always empty)


append : Seq e a -> (() -> Seq e a) -> Seq e a
append =
  Append


error : e -> Seq e a
error =
  Error


-- QUERIES


head : Seq e a -> Maybe (Result e a)
head seq =
  case seq of
    Error e ->
      Just (Err e)

    Nil ->
      Nothing

    Cons first _ ->
      Just (Ok first)

    Append firstSeq makeSecondSeq ->
      case head firstSeq of
        Nothing ->
          head (makeSecondSeq ())

        justX ->
          justX


tail : Seq e a -> Maybe (Result e (Seq e a))
tail seq =
  case seq of
    Error e ->
      Just (Err e)

    Nil ->
      Nothing

    Cons _ makeRestSeq ->
      Just (Ok (makeRestSeq ()))

    Append firstSeq makeSecondSeq ->
      case tail firstSeq of
        Nothing ->
          tail (makeSecondSeq ())

        Just (Err e) ->
          Just (Err e)

        Just (Ok Nil) ->
          Just (Ok (makeSecondSeq ()))

        Just (Ok tailFirstSeq) ->
          Just (Ok (append tailFirstSeq makeSecondSeq))


take : Int -> Seq e a -> Seq e a
take n seq =
  if n == 0 then
    empty
  else
    case head seq of
      Just (Ok first) ->
        Cons first <|
          \_ ->
            case tail seq of
              Just (Ok rest) ->
                take (n - 1) rest

              _ ->
                empty

      _ ->
        empty


-- CONVERSION


toList : Seq e a -> Result e (List a)
toList seq =
  case head seq of
    Nothing ->
      Ok []

    Just (Err e) ->
      Err e

    Just (Ok first) ->
      case tail seq of
        Nothing ->
          Ok [first]

        Just (Err e) ->
          Err e

        Just (Ok tailSeq) ->
          toList tailSeq
            |> Result.map ((::) first)
