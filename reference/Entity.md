# Load and store Entity NBT data

**\[superseded\]**

Entity data (tag 50) holds a list of NBT values for mobs and other
entities in the game. After 1.18.30, entity data was migrated to a new
actor digest format and no longer saved with chunk data.

- `get_entity_value()` and `get_entity_data()` load Entity data from
  `db`. `get_entity_value()` loads data for a single chunk, and
  `get_entity_data()` loads data for multiple chunks.

- `put_entity_value()` and `put_entity_data()` store Entity data for one
  or multiple chunks into `db`.

## Usage

``` r
get_entity_data(x, z, dimension, db = default_db())

get_entity_value(x, z, dimension, db = default_db())

put_entity_data(values, x, z, dimension, db = default_db())

put_entity_value(value, x, z, dimension, db = default_db())
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

`get_entity_value()` returns a list of NBT objects. `get_entity_data()`
returns a named list of lists of NBT objects.
