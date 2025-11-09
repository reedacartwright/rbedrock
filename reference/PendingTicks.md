# Load and store PendingTicks NBT data

PendingTicks data (tag 51) holds a list of NBT values for pending ticks.

## Usage

``` r
get_pending_ticks_data(x, z, dimension, db = default_db())

get_pending_ticks_value(x, z, dimension, db = default_db())

put_pending_ticks_data(values, x, z, dimension, db = default_db())

put_pending_ticks_value(value, x, z, dimension, db = default_db())
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- values:

  A (named) list of list of NBT objects.

- value:

  A list of NBT objects.

## Value

`get_pending_ticks_value()` returns a list of NBT objects.
`get_pending_ticks_data()` returns a named list of lists of NBT objects.

## Details

- `get_pending_ticks_value()` and `get_pending_ticks_data()` load
  PendingTicks data from `db`. `get_pending_ticks_value()` loads data
  for a single chunk, and `get_pending_ticks_data()` loads data for
  multiple chunks.

- `put_pending_ticks_value()` and `put_pending_ticks_data()` store
  PendingTicks data for one or multiple chunks into `db`.
