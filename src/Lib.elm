module Lib exposing (..)
import Html exposing (node, Html)
import List exposing (indexedMap)
import Debug exposing (crash)

noOp : Html a
noOp = node "script" [] []

setCons : a -> List a -> List a
setCons v xs =
  if List.member v xs then xs
  else v::xs

choose : (a -> Maybe b) -> List a -> List b
choose f xs =
  let
    choose_ xs acc =
      case xs of
        [] -> List.reverse acc
        x::rest ->
          case f x of
            Just v -> choose_ rest (v::acc)
            Nothing -> choose_ rest acc
  in
    choose_ xs []

maybeHtml : (List (Html a) -> Html a) -> (b -> Maybe (Html a)) -> List b -> Html a
maybeHtml container func input =
  choose func input |> container

someHtml : (List (Html a) -> Html a) -> (List (Maybe (Html a))) -> Html a
someHtml container html =
  choose (\x -> x) html |> container

elideUnless : Bool -> (List (Html a) -> Html a) -> (List (Maybe (Html a))) -> Html a
elideUnless cond container html =
  if cond then someHtml container html
  else noOp

optionally : Bool -> (() -> a) -> Maybe a
optionally cond v =
  if cond then Just (v ()) else Nothing

removeAt : Int -> List a -> List a
removeAt n list =
  List.take n list ++ List.drop (n+1) list
--  indexedMap (\idx -> \x -> (idx, x))
--    |> List.filter ((idx,v) -> idx /= n)
--    |> List.map ((_,v) -> v)

replaceAt : Int -> a -> List a -> List a
replaceAt n replacement list =
  indexedMap (\idx -> \x -> if idx == n then replacement else x) list

updateAt : Int -> (a -> a) -> List a -> List a
updateAt n update list =
  indexedMap (\idx -> \x -> if idx == n then update x else x) list

swap : Int -> Int -> List a -> List a
swap n m list =
  let
    maxIdx = max n m
    minIdx = min n m
    swap_ before (a,between) curIdx list =
      case (a, list) of
        (Just v, x::rest) ->
          if curIdx == maxIdx then -- this is the one to swap with
            before ++ (x :: List.reverse between) ++ (v :: rest)
          else
            swap_ before (a,x::between) (curIdx+1) rest
        (Just v, []) ->
          crash "swap: Max index in swap doesn't exist"
        (Nothing, []) ->
          crash "swap: Neither max nor min indices exist"
        (Nothing, x::rest) ->
          if curIdx == minIdx then
            swap_ (List.reverse before) (Just x, []) (curIdx+1) rest
          else
            swap_ (x::before) (Nothing, []) (curIdx+1) rest
  in
    if maxIdx == minIdx then list
    else swap_ [] (Nothing,[]) 0 list

nth : Int -> List a -> a
nth n l =
  case l of
    [] -> crash "nth called for an invalid index"
    x::rest -> if n == 0 then x else nth (n-1) rest
