`pokwm` Architecture
====================

`pokwm`'s division into modules is hopefully somewhat logical.  The current
purpose of each is:

### P

`P.hs` contains the `P` monad, which is similar in structure to the
more well-known `X` monad.  It contains a static configuration as a `ReaderT`
and dynamic state as a `StateT` wrapped around `IO`.

It contains various functions to inject into the monad, and various convenient
state wrappers.

### Operations

Contains high-level operations for communication with X and other general
operations.  Most importantly, these are and should be independent of the
underlying model we use for the current window layout state.

### WindowSplit

Contains our model of how the windows are currently split and operations on
the model, independent of the actual communication to X.  This is probably
the most volatile part.

### Controls

X operations that are not independent of the window layout state.  This is
the glue between Operations and WindowSplit, and should change appropriately
with changes in WindowSplit.  Ideally, user inputs that change the model
should make a call into Controls.

### Keys

Defines the keymapping.

### Main

Gets X's attention, sets up handlers, and injects initial state and config
into `P`, where we ideally stay.
