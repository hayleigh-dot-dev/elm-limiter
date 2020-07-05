module Limiter.Internals exposing
  ( Limiter(..)
  , Mode(..), State(..), Msg(..)
  , emit, emitAfter
  )

import Task
import Process

{-| -}
type Limiter msg
    = Limiter
        { tagger : Msg msg -> msg
        , mode : Mode msg
        , state : State
        }

{-| The `Debounce` mode keeps track of how many messages it's received in a
particular burst. Every message added to the list schedules a check some time
in the future; if the list hasn't changed in that time we emit the newest
message in the list and discard the rest.

The `Throttle` mode just needs to keep track of what interval to throttle
messages at.

Both modes expect the time to be in _milliseconds_.

-}
type Mode msg
    = Debounce Int (List msg)
    | Throttle Int

{-| -}
type State
    = Open
    | Closed

{-| -}
type Msg msg
    = Emit msg
    | EmitIfSettled Int
    | None
    | Reopen
    | Push msg

{-| Take a msg and turn it into a command that is resolved after a specified
delay.
-}
emitAfter : Int -> msg -> Cmd msg
emitAfter delay msg =
    Basics.toFloat delay
        |> Process.sleep
        |> Task.perform (always msg)


{-| Take a msg and turn it into a command that is immediately resolved. This
should result in a single render happening between the call to `emit` and
receiving the actual msg.

There is no way to combat this, as commands are asynchronous by nature.

-}
emit : msg -> Cmd msg
emit msg =
    Task.succeed msg
        |> Task.perform identity
