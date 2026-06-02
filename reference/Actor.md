# Read and write Actor data

The nbt data of a actor is saved in the database, using a key with a
prefix and a 16-character storage key: 'actor:0123456789abcdef'.

## Usage

``` r
get_actor_value(id, db = default_db())

get_actor_data(ids, db = default_db())

put_actor_value(value, dimension, db = default_db())

put_actor_data(values, dimension, db = default_db())

make_actor_keys(ids)

get_actor_keys(db = default_db())
```
