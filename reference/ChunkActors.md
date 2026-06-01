# Read and write Actor data for a chunk

The nbt data of each actor is saved independently in the database, using
a key with a prefix and a 16-character storage key:
'actor:0123456789abcdef'. The keys of all actors in a chunk are saved in
an
[ActorDigest](https://reedacartwright.github.io/rbedrock/reference/ActorDigest.md)
record, with format acdig:x:z:dimension'.

## Usage

``` r
get_chunk_actors_value(x, z, dimension, db = default_db())

get_chunk_actors_data(x, z, dimension, db = default_db())

put_chunk_actors_data(values, x, z, dimension, db = default_db())

put_chunk_actors_value(value, x, z, dimension, db = default_db())

make_actor_keys(ids)
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

- ids:

  A vector of UniqueIDs.

## Details

`get_chunk_actors_value()` loads Actors data for a single chunk in `db`.
`get_chunk_actors_data()` loads Actors data from multiple chunks in
`db`.

`put_chunk_actors_value()` and `put_chunk_actors_data()` store
one/multiple chunks Actors data into `db` and update the chunks'
ActorDigests. When storing Actors data, an actor's storage key will be
recalculated from the actor's `UniqueID`. The actor's position and
dimension are not verified to be in the chunk it is assigned to.

`make_actor_keys()` creates actor keys based on UniqueIDs.

## See also

[ActorDigest](https://reedacartwright.github.io/rbedrock/reference/ActorDigest.md)
