# Read and manipulate chunk keys

Chunk keys are keys to chunk data. A chunk key has a format which
indicates the chunk it holds data for and the type of data it holds.
This format is either `chunk:x:z:d:t` or `chunk:x:z:d:t:s`, where `x`
and `z` indicates the coordinates of the chunk in chunk space, `d`
indicates the dimension of the chunk, and `t` and `s` indicate the tag
and subtag of the chunk.

`parse_chunk_keys()` splits chunk keys into their individual elements
and returns a table with the results.

`create_chunk_keys()` returns a vector of chunk keys formed from its
arguments.

`chunk_positions()` returns a matrix containing the chunk coordinates of
keys.

`chunk_origins()` returns a matrix containing the block coordinate of
the NW corner of keys.

`chunk_tag_str()` and `chunk_tag_int()` convert between integer and
character representations of chunk tags.

## Usage

``` r
parse_chunk_keys(keys)

create_chunk_keys(x, z, dimension, tag, subtag)

chunk_positions(keys)

chunk_origins(keys)

chunk_tag_str(tags)

chunk_tag_int(tags)
```

## Arguments

- keys:

  A character vector of database keys.

- x:

  Chunk x-coordinate.

- z:

  Chunk z-coordinate.

- dimension:

  Dimension.

- tag:

  The type of chunk data.

- subtag:

  The subchunk the key refers to (Only used for tag 47).

- tags:

  a vector

## Examples

``` r
parse_chunk_keys("chunk:0:0:0:44")
#> # A tibble: 1 × 6
#>   key                x     z dimension tag     subtag
#>   <chr>          <int> <int>     <int> <chr>    <int>
#> 1 chunk:0:0:0:44     0     0         0 Version     NA
parse_chunk_keys("chunk:0:0:0:47:1")
#> # A tibble: 1 × 6
#>   key                  x     z dimension tag            subtag
#>   <chr>            <int> <int>     <int> <chr>           <int>
#> 1 chunk:0:0:0:47:1     0     0         0 SubChunkBlocks      1
create_chunk_keys(0, 0, 0, 47, 1)
#> [1] "chunk:0:0:0:47:1"
```
