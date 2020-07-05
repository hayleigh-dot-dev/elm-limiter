module Limiter.Events exposing
  ( event
  , onClick, onInput
  )

{-| 

@docs event

@docs onClick, onInput

-}

import Html
import Html.Events
import Limiter.Internals exposing 
  ( Limiter(..)
  , Mode(..), State(..), Msg(..)
  )


{-|

    Html.button
        [ Html.Events.onClick (Limiter.event Increment model.debouncer) ]
        [ Html.text "+" ]

-}
event : msg -> Limiter msg -> msg
event msg (Limiter { tagger, mode, state }) =
    case ( state, mode ) of
        ( Open, Debounce _ _ ) ->
            tagger (Push msg)

        ( Open, Throttle _ ) ->
            tagger (Emit msg)

        ( Closed, _ ) ->
            tagger None

{-|

    Html.button
        [ Limiter.Events.onClick Increment model.debouncer ]
        [ Html.text "+" ]

-}
onClick : msg -> Limiter msg -> Html.Attribute msg
onClick msg limiter =
    Html.Events.onClick (event msg limiter)

{-| -}
onInput : (String -> msg) -> Limiter msg -> Html.Attribute msg
onInput msg limiter =
    Html.Events.onInput (\s -> event (msg s) limiter)
