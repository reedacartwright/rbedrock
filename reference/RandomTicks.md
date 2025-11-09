# Load and store RandomTicks NBT data

RandomTicks data (tag 59) holds a list of NBT values for random ticks.

## Usage

``` r
get_random_ticks_data(x, z, dimension, db = default_db())

get_random_ticks_value(x, z, dimension, db = default_db())

put_random_ticks_data(values, x, z, dimension, db = default_db())

put_random_ticks_value(value, x, z, dimension, db = default_db())
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

`get_random_ticks_value()` returns a list of NBT objects.
`get_random_ticks_data()` returns a named list of lists of NBT objects.

## Details

- `get_random_ticks_value()` and `get_random_ticks_data()` load
  RandomTicks data from `db`. `get_random_ticks_value()` loads data for
  a single chunk, and `get_random_ticks_data()` loads data for multiple
  chunks.

- `put_random_ticks_value()` and `put_random_ticks_data()` store
  RandomTicks data for one or multiple chunks into `db`.
