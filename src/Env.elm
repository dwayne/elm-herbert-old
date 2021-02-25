module Env exposing (Env, empty, extend, lookup)


type Env k v
  = Env (List (k, v))


empty : Env k v
empty =
  Env []


extend : k -> v -> Env k v -> Env k v
extend name value (Env bindings) =
  Env ((name, value) :: bindings)


lookup : k -> Env k v -> Maybe v
lookup searchName (Env bindings) =
  let
    helper pairs =
      case pairs of
        [] ->
          Nothing

        ((name, value) :: rest) ->
          if searchName == name then
            Just value
          else
            helper rest
  in
  helper bindings
