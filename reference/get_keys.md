# Get a list of keys stored in a bedrockdb.

Get a list of keys stored in a bedrockdb.

## Usage

``` r
get_keys(prefix = NULL, db = default_db(), readoptions = NULL)
```

## Arguments

- prefix:

  A string specifying chunk prefix or string prefix.

- db:

  A `bedrockdb` object

- readoptions:

  A `bedrock_leveldb_readoptions` object

## Value

A vector containing all the keys found in the bedrockdb.

If `prefix` is specified, this vector will be filtered for based on the
specified prefix.
