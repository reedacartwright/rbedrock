# Random Number Generation for Minecraft

Bedrock Edition's central random number algorithm is MT19937. However,
R's MT19937 code is not compatible with Bedrock's. These routines
provide an API that is compatible with Bedrock's.

`bedrock_random_seed()` seeds the random number generator.

`bedrock_random_state()` returns the current state of the random number
generator as a raw vector.

`bedrock_random_get_uint()` returns a 32-bit random integer. Default
range is `[0, 2^32-1]`.

`bedrock_random_get_int()` returns a 31-bit random integer. Default
range is `[0, 2^31-1]`.

`bedrock_random_get_float()` returns a random real number. Default range
is `[0.0, 1.0)`.

`bedrock_random_get_double()` returns a random real number Default range
is `[0.0, 1.0)`.

## Usage

``` r
bedrock_random_seed(value)

bedrock_random_state(new_state = NULL)

bedrock_random_get_uint(n, max)

bedrock_random_get_int(n, min, max)

bedrock_random_get_float(n, min, max)

bedrock_random_get_double(n)
```

## Arguments

- value:

  a scalar integer

- new_state:

  a raw vector

- n:

  number of observations.

- min, max:

  lower and upper limits of the distribution. Must be finite. If only
  one is specified, it is taken as `max`. If neither is specified, the
  default range is used.

## Examples

``` r
# seed the global random number generator
bedrock_random_seed(5490L)
#> NULL

# save and restore rng state
saved_state <- bedrock_random_state()
bedrock_random_get_uint(10)
#>  [1] 2248850472 2838390091  333212849 1904364082  893966104 3694621526
#>  [7]  971023934 3050836922 1272704213 4283041477
bedrock_random_state(saved_state)
bedrock_random_get_uint(10)
#>  [1] 2248850472 2838390091  333212849 1904364082  893966104 3694621526
#>  [7]  971023934 3050836922 1272704213 4283041477
```
