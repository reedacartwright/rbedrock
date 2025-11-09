# Open a Bedrock Edition world for reading and writing.

`bedrockdb` opens a handle to a leveldb database that contains save-game
data for a Bedrock Edition world. On success, it returns an R6 class of
type 'bedrockdb' that can be used directly for low-level reading and
writing access to the db or can be passed to higher-level functions. The
handle to the database can be closed by passing it to `close`.

## Usage

``` r
bedrockdb(
  path,
  create_if_missing = FALSE,
  error_if_exists = NULL,
  paranoid_checks = NULL,
  write_buffer_size = 4194304L,
  max_open_files = NULL,
  block_size = 163840L,
  cache_capacity = 83886080L,
  bloom_filter_bits_per_key = 10L,
  compression_level = -1L
)

# S3 method for class 'bedrockdb'
close(con, compact = FALSE, ...)

is_bedrockdb(x)
```

## Arguments

- path:

  The path to a world folder. If the path does not exist, it is assumed
  to be the base name of a world folder in the local minecraftWorlds
  directory.

- create_if_missing:

  Create world database if it doesn't exist.

- error_if_exists:

  Raise an error if the world database already exists.

- paranoid_checks:

  Internal leveldb option

- write_buffer_size:

  Internal leveldb option

- max_open_files:

  Internal leveldb option

- block_size:

  Internal leveldb option

- cache_capacity:

  Internal leveldb option

- bloom_filter_bits_per_key:

  Internal leveldb option

- compression_level:

  Internal leveldb option

- con:

  An database object created by bedrockdb.

- compact:

  Compact database before closing.

- ...:

  arguments passed to or from other methods.

- x:

  An object.

## Value

On success, `bedrockdb` returns an R6 class of type 'bedrockdb'.

## Examples

``` r
# open an example works and get all keys
dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)
keys <- get_keys()
close(db)

if (FALSE) { # \dontrun{

# open a world in the minecraftWorlds folder using a world id.
db <- bedrockdb("lrkkYFpUABA=")
# do something with db ...
close(db)

# open a world using absolute path
db <- bedrockdb("C:\\\\minecraftWorlds\\\\my_world")
# do something with db ...
close(db)
} # }
```
