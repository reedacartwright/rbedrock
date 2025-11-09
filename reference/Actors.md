# Read and write Actor data

After 1.18.30, the nbt data of each actor is saved independently in the
database, using a key with a prefix and a 16-character storage key:
'actor:0123456789abcdef'. The keys of all actors in a chunk are saved in
an
[ActorDigest](https://reedacartwright.github.io/rbedrock/reference/ActorDigest.md)
record, with format acdig:x:z:dimension'.

## Usage

``` r
get_actors_data(x, z, dimension, db = default_db())

get_actors_value(x, z, dimension, db = default_db())

put_actors_data(values, x, z, dimension, db = default_db())

put_actors_value(value, x, z, dimension, db = default_db())
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

  A list of nbt actors data

## Details

`get_actors_value()` loads Actors data for a single chunk in `db`.
`get_actors_data()` loads Actors data from multiple chunks in `db`.

`put_actors_value()` and `put_actors_data()` store one/multiple chunks
Actors data into `db` and update the chunks' ActorDigests. When storing
Actors data, an actor's storage key will be recalculated from the
actor's `UniqueID`. The actor's position and dimension are not verified
to be in the chunk it is assigned to.

## See also

[ActorDigest](https://reedacartwright.github.io/rbedrock/reference/ActorDigest.md),
[Entity](https://reedacartwright.github.io/rbedrock/reference/Entity.md)
