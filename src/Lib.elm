module Lib exposing
    ( chainUpdate
    , choose
    , elideIf
    , elideUnless
    , indexedChoose
    , maybeHtml
    , noOp
    , nth
    , optionally
    , removeAt
    , replaceAt
    , setCons
    , someHtml
    , swap
    , updateAt
    , widget
    )

import Button exposing (..)
import Debug exposing (log)
import Html exposing (Html, node, span)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import List exposing (indexedMap)



{--
Buttons, for view.  Here, because I'd like a consistent widget set,
with consistent titles etc
--}


buttonToString : Button -> ( String, String )
buttonToString v =
    case v of
        MoveLeft ->
            ( "fas fa-arrow-alt-circle-left widget moveleft", "Move left" )

        MoveRight ->
            ( "fas fa-arrow-alt-circle-right widget moveright", "Move right" )

        Edit s ->
            ( "fas fa-edit widget edit", "Change " ++ s )

        Delete s ->
            ( "fas fa-trash widget delete", "Delete " ++ s )

        Confirm ->
            ( "fas fa-check widget confirm", "Confirm" )

        Cancel ->
            ( "fas fa-times widget cancel", "Cancel" )

        Append s ->
            ( "fas fa-plus-circle widget append", "Add new " ++ s )

        Male ->
            ( "fas fa-mars widget male", "Male" )

        Female ->
            ( "fas fa-venus widget female", "Female" )

        Big btn ->
            let
                ( class, s ) =
                    buttonToString btn
            in
            ( "fa-2x " ++ class, s )

        Huge btn ->
            let
                ( class, s ) =
                    buttonToString btn
            in
            ( "fa-4x " ++ class, s )


widget : Button -> a -> Html a
widget btn click =
    let
        ( class_, title_ ) =
            buttonToString btn
    in
    span [ class class_, title title_, onClick click ] []


{-| A "no-op" element in HTML: an empty <script></script>.
-}
noOp : Html a
noOp =
    node "script" [] []


{-| "Cons, as if the list was a set". Will cons a value to a list, if that
value does not already exist
-}
setCons : a -> List a -> List a
setCons v xs =
    if List.member v xs then
        xs

    else
        v :: xs


chooseHelper : (a -> Maybe b) -> List a -> List b -> List b
chooseHelper f xs acc =
    case xs of
        [] ->
            List.reverse acc

        x :: rest ->
            case f x of
                Just v ->
                    chooseHelper f rest (v :: acc)

                Nothing ->
                    chooseHelper f rest acc


choose : (a -> Maybe b) -> List a -> List b
choose f xs =
    chooseHelper f xs []


indexedChooseHelper f idx xs acc =
    case xs of
        [] ->
            List.reverse acc

        x :: rest ->
            case f idx x of
                Just v ->
                    indexedChooseHelper f (idx + 1) rest (v :: acc)

                Nothing ->
                    indexedChooseHelper f (idx + 1) rest acc


indexedChoose : (Int -> a -> Maybe b) -> List a -> List b
indexedChoose f xs =
    indexedChooseHelper f 0 xs []


{-| Take a container element, a function, and some input list.
Apply the function to each element of the list. When the function gives back
a `Maybe (Html a)`, include it in the container; otherwise, discard it.
-}
maybeHtml : (List (Html a) -> Html a) -> (b -> Maybe (Html a)) -> List b -> Html a
maybeHtml container func input =
    choose func input |> container


{-| Take a container element and a list of `Maybe (Html a)` elements; only
include the content elements that are `Just`. Interacts well with `optionally`.

Example:

someHtml (div [class "Whatever"])
[ optionally (ifThisIsOK var) (() -> span [class "moo"])
, optionally whenMoonIsFull bigRedButton
][ optionally (ifThisIsOK var) (() -> span [class "moo"] [])
, optionally whenMoonIsFull bigRedButton
]

-}
someHtml : (List (Html a) -> Html a) -> List (Maybe (Html a)) -> Html a
someHtml container html =
    choose (\x -> x) html |> container


{-| Elide a container element (using `noOp`), unless the `cond` is true. If
the `cond` is true, call `someHtml` with the `container` and `html`

Example:

elideUnless someBoolean (div [class "container-class"])
[ optionally (ifThisIsOK var) (() -> span [class "moo"])
, optionally whenMoonIsFull bigRedButton
][ optionally (ifThisIsOK var) (() -> span [class "moo"] [])
, optionally whenMoonIsFull bigRedButton
]

-}
elideUnless : Bool -> (List (Html a) -> Html a) -> List (Maybe (Html a)) -> Html a
elideUnless cond container html =
    if cond then
        someHtml container html

    else
        noOp


{-| Elide a container element (using `noOp`), if the `cond` is true. If
the `cond` is false, call `someHtml` with the `container` and `html`

Example:

elideIf someBoolean (div [class "container-class"])
[ optionally (ifThisIsOK var) (() -> span [class "moo"])
, optionally whenMoonIsFull bigRedButton
][ optionally (ifThisIsOK var) (() -> span [class "moo"] [])
, optionally whenMoonIsFull bigRedButton
]

-}
elideIf : Bool -> (List (Html a) -> Html a) -> List (Maybe (Html a)) -> Html a
elideIf cond container html =
    if cond then
        noOp

    else
        someHtml container html


{-| If `cond`, execute `v` to produce a `Maybe a`. Otherwise, `Nothing`. Can
be used very effectively with `someHtml`, `elideUnless`, etc.
-}
optionally : Bool -> (() -> a) -> Maybe a
optionally cond v =
    if cond then
        Just (v ())

    else
        Nothing


removeAt : Int -> List a -> List a
removeAt n list =
    List.take n list ++ List.drop (n + 1) list


replaceAt : Int -> a -> List a -> List a
replaceAt n replacement list =
    indexedMap
        (\idx ->
            \x ->
                if idx == n then
                    replacement

                else
                    x
        )
        list


updateAt : Int -> (a -> a) -> List a -> List a
updateAt n update list =
    indexedMap
        (\idx ->
            \x ->
                if idx == n then
                    update x

                else
                    x
        )
        list


swap : Int -> Int -> List a -> Maybe (List a)
swap n m list =
    let
        maxIdx =
            max n m

        minIdx =
            min n m

        swap_ before ( a, between ) curIdx xs =
            case ( a, xs ) of
                ( Just v, x :: rest ) ->
                    if curIdx == maxIdx then
                        -- this is the one to swap with
                        Just (before ++ (x :: List.reverse between) ++ (v :: rest))

                    else
                        swap_ before ( a, x :: between ) (curIdx + 1) rest

                ( Just v, [] ) ->
                    log "swap: Max index in swap doesn't exist" Nothing

                ( Nothing, [] ) ->
                    log "swap: Neither max nor min indices exist" Nothing

                ( Nothing, x :: rest ) ->
                    if curIdx == minIdx then
                        swap_ (List.reverse before) ( Just x, [] ) (curIdx + 1) rest

                    else
                        swap_ (x :: before) ( Nothing, [] ) (curIdx + 1) rest
    in
    if maxIdx == minIdx then
        Just list

    else
        swap_ [] ( Nothing, [] ) 0 list


nth : Int -> List a -> Maybe a
nth n l =
    case l of
        [] ->
            log "nth called for an invalid index" Nothing

        x :: rest ->
            if n == 0 then
                Just x

            else
                nth (n - 1) rest


chainUpdate : ( model, Cmd msg ) -> (msg -> model -> ( model, Cmd msg )) -> msg -> ( model, Cmd msg )
chainUpdate ( in_model, in_c ) update msg =
    let
        ( updated, c ) =
            update msg in_model
    in
    ( updated, Cmd.batch [ in_c, c ] )
