# Remove values from a bedrockdb.

Remove values from a bedrockdb.

## Usage

``` r
delete_values(
  keys,
  db = default_db(),
  report = FALSE,
  readoptions = NULL,
  writeoptions = NULL
)
```

## Arguments

- keys:

  A character vector of keys.

- db:

  A `bedrockdb` object

- report:

  A logical indicating whether to generate a report on deleted keys

- readoptions:

  A `bedrock_leveldb_readoptions` object

- writeoptions:

  A `bedrock_leveldb_writeoptions` object

## Value

If `report == TRUE`, a logical vector indicating which keys were
deleted.
