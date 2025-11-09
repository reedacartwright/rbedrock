# Read values stored in a bedrockdb.

Read values stored in a bedrockdb.

## Usage

``` r
key_prefix(prefix)

starts_with(prefix)

get_data(keys, db = default_db(), readoptions = NULL)

get_value(key, db = default_db(), readoptions = NULL)

has_values(keys, db = default_db(), readoptions = NULL)
```

## Arguments

- prefix:

  A string specifying key prefix

- keys:

  A character vector of keys

- db:

  A `bedrockdb` object

- readoptions:

  A `bedrock_leveldb_readoptions` object

- key:

  A single key

## Value

get_data()`returns a named-list of raw vectors.`get_value()\` returns a
raw vector.

`has_values()` returns a logical vector.
