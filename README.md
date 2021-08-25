# elm-limiter

> Throttling and debouncing for messages and values.

## A Complete Example

```elm
module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import Limiter exposing (Limiter)


{- Main --------------------------------------------------------------------- -}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


{- Model -------------------------------------------------------------------- -}
type alias Model =
    { counter : Int
    , rawInput : String
    , debouncer : Limiter Msg
    , throttler : Limiter Msg
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { counter = 0
      , rawInput = ""
      , debouncer = Limiter.debounce DebounceMsg 500
      , throttler = Limiter.throttle ThrottleMsg 100
      }
    , Cmd.none
    )


{- Update ------------------------------------------------------------------- -}
type Msg
    = Increment
    | GotInput String
    | SearchFor String
    | DebounceMsg (Limiter.Msg Msg)
    | ThrottleMsg (Limiter.Msg Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg modl =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )

        GotInput input ->
            Limiter.push (SearchFor input) model.throttler
                |> Tuple.mapFirst
                    (\throttler ->
                        { model
                            | throttler = throttler
                            , rawInput = input
                        }
                    )

        SearchFor input ->
            ( model
              -- Imagine we wanted to search for something on the backend. We'd
              -- be sending unnecessary requests if we search every keypress!
            , searchBackendFor input
            )

        DebounceMsg debounceMsg ->
            Limiter.update debounceMsg model.debouncer
                |> Tuple.mapFirst (\debouncer -> { model | debouncer = debouncer })

        ThrottlerMsg throttleMsg ->
            Limiter.update throttleMsg model.throtther
                |> Tuple.mapFirst (\throttler -> { model | throttler = throttler })


{- View --------------------------------------------------------------------- -}
view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button
                [ Html.Events.onClick (Limiter.event Increment model.debouncer) ]
                [ Html.text "+" ]
            , Html.p []
                [ Html.text <| String.fromInt model.counter ]
            ]
        , Html.div []
            [ Html.span []
                [ Html.text "Search for: " ]
            , Html.input
                [ Html.Events.onInput GotInput
                , Html.Attributes.value model.rawInpt
                ] []
            ]
        ]
```
