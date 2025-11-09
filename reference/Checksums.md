# Load and store Checksums data

**\[superseded\]**

Checksums data (tag 59) holds checksums for several chunk records. These
records are 2DMaps (tag 45), SubchunkBlocks (tag 47), BlockEntities (tag
49), and Entities (tag 50). Checksums are no longer used by the game.

- `get_checksums_value()` and `get_checksums_data()` load Checksums data
  from `db`. `get_checksums_value()` loads data for a single chunk, and
  `get_checksums_data()` loads data for multiple chunks.

- `update_checksums_value()` and `updates_checksums_data()` recalculate
  Checksums data from `db` and update `db`.

- `write_checksums_value()` encodes Checksums data into a raw vector.
  `read_checksums_value()` decodes binary Checksums data.

## Usage

``` r
get_checksums_value(x, z, dimension, db = default_db())

get_checksums_data(x, z, dimension, db = default_db())

update_checksums_value(x, z, dimension, db = default_db())

update_checksums_data(x, z, dimension, db = default_db())

read_checksums_value(rawvalue, key)

write_checksums_value(value)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- rawvalue:

  A raw vector.

- key:

  A key prefix.

- value:

  A Checksums value.

## Value

`get_checksums_value()` returns a Checksums value.
`get_checksums_data()` returns a named list of Checksums values.
Checksums values are named character vectors.
