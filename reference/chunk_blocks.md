# Extract or replace chunk blocks from an array

Convenience wrappers around `[` to extract or replace blocks from an
array based on block coordinates.

## Usage

``` r
chunk_blocks(x, ..., drop = TRUE, origin = chunk_origin(x))

chunk_blocks(x, ..., origin = chunk_origin(x)) <- value
```

## Arguments

- x:

  Object from which to extract element(s) or in which to replace
  element(s).

- ...:

  block coordinates specifying elements to extract or replace. Can be
  numeric, logical, or missing. If numeric, the coordinates will be
  mapped to indices unless there is a single, non-matrix argument.

- drop:

  if `TRUE` the result is coerced to the lowest possible dimension.

- origin:

  the origin of the chunk array, used for mapping coordinates to indices

- value:

  An array-like R object of similar class as x
