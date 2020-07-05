module Limiter exposing
    ( Limiter, Msg
    , debounce, throttle
    , push
    , update
    )

{-| A Limiter is a handy way of slowing down the number of messages or events
we get from a particular source. We have two main methods of rate-limiting this
data:

  - Debouncing
  - Throttling

A debouncer will wait a short delay after a burst of inputs and then emit the
most recent message. On the other hand, a throttler will typically emit the
first message from a burst and then emit a second message after a fixed time
period has passed, and then a third after that time period again and so on.

You can visualise the difference like so:

  - Debouncing

```
--a-b-cd--e----------f--------
---------------e----------f---


```

  - Throttling

```
--a-b-cd--e----------f--------
--a---c---e----------f--------


```

There are also two ways to limit messages with this module. We can limit messages
directly from a HTML event or we can manually push messages into the Limiter,
typically inside our `update` function. Which approach you take is typically
decided on whether you need to save some information from the event directly.

If the answer is no, as is the case with click events on buttons and similar
elements, then limiting at the source of the event using `Limiter.event` is
the cleanest solution. If the answer is yes, as is potentially the case with
text inputs, then you're likely to want `Limiter.push` so you can save the
raw input text in real-time and rate-limit some reaction to those events.

A simple but complete example demonstrating how both types on limiter and
both approaches to limiting can be used is found at the bottom of this page,
after the documentation.


# The Limiter type

@docs Limiter, Msg


# Constructing a Limiter

@docs debounce, throttle


# Adding events to a Limiter

@docs push


# Updating a Limiter

@docs update

-}

import Limiter.Internals exposing 
  ( Limiter(..)
  , Mode(..), State(..), Msg(..)
  , emit, emitAfter
  )


{- Types -------------------------------------------------------------------- -}
{-| The `Limiter` type wraps up all the data necessary to properly rate-limit
incoming messages. You will need a separate Limiter for every event/message
you want to limit (unless you want to share the same timing across different
events).

This means you'll need to store each Limiter in your model.

-}
type alias Limiter msg
    = Limiter.Internals.Limiter msg

{-| A type for messages internal to the Limiter. Notice how it is parameterised
by the `msg` type you want to rate-limit (although this could be any time, not
just a message).

These just get passed into the Limiter in your `update` function.

-}
type alias Msg msg
    = Limiter.Internals.Msg msg


{- Creating a Limiter ------------------------------------------------------- -}
{-| A debouncer limits messages by waiting for a burst of messages to settle
before emitting the most recent message. This means they'll always be a brief
delay even if only one message is received; this is demonstrated below.

    --a-b-cd--e----------f--------
    ---------------e----------f---



To construct a debouncer you need to pass in a "tagger" function that will wrap
the Limiter's internal `Msg` type. You also need to supply the cooldown time
in milliseconds which is the delay between the last message in a burst being
sent and that message being emitted.

-}
debounce : (Msg msg -> msg) -> Int -> Limiter msg
debounce tagger cooldown =
    Limiter
        { tagger = tagger
        , mode = Limiter.Internals.Debounce cooldown []
        , state = Limiter.Internals.Open
        }


{-| A throttler limits messages by only alowwing messages to come in as fast
as a fixed interval allows. When receive a burst of messages, the first one
will pass through the emitter and then all messages are ignored for a period of
time, then the next message will pass through and so on.

    --a-b-cd--e----------f--------
    --a---c---e----------f--------



To construct a debouncer you need to pass in a "tagger" function that will wrap
the Limiter's internal `Msg` type. You also need to supply the interval time in
milliseconds, which is the minimum amount of time that must pass before
consecutive messages can be emitted.

-}
throttle : (Msg msg -> msg) -> Int -> Limiter msg
throttle tagger interval =
    Limiter
        { tagger = tagger
        , mode = Throttle interval
        , state = Open
        }



{-| Sometimes we don't want to limit events coming from the HTML, but we want
to limit how often we perform an action based on that event instead. A typical
example is searching for things on the backend in realtime as the user types.

We don't want to send a HTTP request on each keypress, but we also don't want
to limit the events coming from the HTML otherwise we'll lose the user's input.
In these cases we use `Limiter.push` to manually push messages into the
Limiter.

What we get back is a tuple with a new Limiter and a `Cmd` that will immediately
resolve if the message we pushed in was allowed through.

As a rule of thumb, if you _do_ need to capture some data from the event source
then `Limiter.push` will allow you to store that data before rate-limiting some
additional message in response.

    update msg model =
        case msg of
            GotInput input ->
                Limiter.push (SearchFor input) model.throttler
                    |> Tuple.mapFirst
                        (\throttler ->
                            { model
                                | throttler = throttler
                                , input = input
                            }
                        )

            SearchFor input ->
                ( model
                , Http.get
                    { ...
                    }
                )

-}
push : msg -> Limiter msg -> ( Limiter msg, Cmd msg )
push msg (Limiter ({ tagger, mode, state } as limiter)) =
    case ( state, mode ) of
        ( Open, Debounce cooldown queue ) ->
            ( Limiter { limiter | mode = Debounce cooldown (msg :: queue) }
            , emitAfter cooldown (tagger <| EmitIfSettled (List.length queue + 1))
            )

        ( Open, Throttle interval ) ->
            ( Limiter { limiter | state = Closed }
            , Cmd.batch
                [ emitAfter interval (tagger Reopen)
                , emit msg
                ]
            )

        ( Closed, _ ) ->
            ( Limiter limiter
            , Cmd.none
            )


{- Update ------------------------------------------------------------------- -}
{-| Limiters work by producing their own `Msg` values and using them to update
the internal state of the Limiter. When you construct a Limiter you have to
provide a "tagger" function that wraps these internal messages into a type
that your own update function can deal with.

You don't need to do much with this function, just ensure that you're calling
it in your update function whenever you get a wrapper message:

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            ...

            LimiterMsg limiterMsg ->
                Limiter.update limiterMsg model.limiter
                    |> Tuple.mapFirst (\limiter -> { model | limiter = limiter })

The update function produces Cmds like any other update function might, and this
is how the messages we care about make their way into our application. You will
need to repeat the little bit of code above for every Limiter you have in an
application.

-}
update : Msg msg -> Limiter msg -> ( Limiter msg, Cmd msg )
update internalMsg (Limiter ({ tagger, mode, state } as limiter)) =
    case ( internalMsg, state, mode ) of
        ( Emit msg, Open, Throttle interval ) ->
            ( Limiter { limiter | state = Closed }
            , Cmd.batch
                [ emitAfter interval (tagger Reopen)
                , emit msg
                ]
            )

        ( EmitIfSettled msgCount, Open, Debounce cooldown queue ) ->
            if List.length queue == msgCount then
                ( Limiter { limiter | mode = Debounce cooldown [] }
                , List.head queue
                    |> Maybe.map emit
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( Limiter limiter
                , Cmd.none
                )

        ( Reopen, _, _ ) ->
            ( Limiter { limiter | state = Open }
            , Cmd.none
            )

        ( Push msg, Open, Debounce cooldown queue ) ->
            ( Limiter { limiter | mode = Debounce cooldown (msg :: queue) }
            , emitAfter cooldown (tagger <| EmitIfSettled (List.length queue + 1))
            )

        _ ->
            ( Limiter limiter
            , Cmd.none
            )
