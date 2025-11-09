# Read and write Actor Digest Data

Actor digests store a list of all entities in a chunk; however they are
not chunk data and use their own prefix. The key format for actor digest
data is acdig:x:z:dimension.

`get_acdig_data()` and `get_acdig_value()` load ActorDigest data from
`db`. `get_acdig_value()` supports loading only a single value.

`put_acdig_data()` and `put_acdig_value()` store ActorDigest data into
`db`.

`read_acdig_value()` and `write_acdig_value()` decode and encode
ActorDigest data respectively.

`create_acdig_keys()` creates keys for ActorDigest data.

## Usage

``` r
get_acdig_data(x, z, dimension, db = default_db())

get_acdig_value(x, z, dimension, db = default_db())

put_acdig_data(values, x, z, dimension, db = default_db())

put_acdig_value(value, x, z, dimension, db = default_db())

read_acdig_value(rawvalue)

write_acdig_value(value)

create_acdig_keys(x, z, dimension)
```

## Arguments

- x, z, dimension:

  Chunk coordinates to extract data from. `x` can also be a character
  vector of db keys.

- db:

  A bedrockdb object.

- values:

  A list of character vectors. If `x` is missing, the names of `values`
  will be taken as the keys.

- value:

  A character vector.

- rawvalue:

  A raw vector.

## Value

`get_acdig_values()` returns a vector of actor keys. `get_acdig_data()`
returns a named list of the of the values returned by
`get_acdig_value()`.

## See also

[Actors](https://reedacartwright.github.io/rbedrock/reference/Actors.md),
[Entity](https://reedacartwright.github.io/rbedrock/reference/Entity.md)
