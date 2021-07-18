test_that("default bedrock_random seed is 5489", {
    state_default <- bedrock_random_state()

    bedrock_random_seed(5489L)
    state_5489 <- bedrock_random_state()

    expect_equal(state_default, state_5489)
})

test_that("the 10,000th value of the default-constructed bedrock_random is 4123659995", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_uint(10000)
    expect_equal(values[10000], 4123659995)
})

test_that("bedrock_random_state() restores state", {
    bedrock_random_seed(5490L)

    saved_state <- bedrock_random_state()
    values1 <- bedrock_random_get_uint(10000)
    state1 <- bedrock_random_state(saved_state)

    values2 <- bedrock_random_get_uint(10000)
    state2 <- bedrock_random_state()

    expect_equal(values1, values2)
    expect_equal(state1, state2)
})

test_that("bedrock_random_uint32 works", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_uint(10000)
    expect_equal(values[10000], 4123659995)
})

test_that("bedrock_random_uint32 works with a maximum value", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_uint(10000, 17)
    expect_equal(values[10000], 4123659995 %% 17)
})

test_that("bedrock_random_int32 works", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_int(10000)
    expect_equal(values[10000], 4123659995 %/% 2)
})

test_that("bedrock_random_int32 works with a maximum value", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_int(10000, 17)
    expect_equal(values[10000], 4123659995 %% 17)

    values <- bedrock_random_get_int(10,0)
    expect_equal(values, rep(0,10L))
})

test_that("bedrock_random_int32 works with a minimum and a maximum value", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_int(10000, 10, 17)
    expect_equal(values[10000], (4123659995 %% 7) + 10)

    values <- bedrock_random_get_int(10,20,20)
    expect_equal(values, rep(20,10L))

    values <- bedrock_random_get_int(10,20,10)
    expect_equal(values, rep(20,10L))
})

test_that("bedrock_random_double works", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_double(10000)
    expect_equal(values[10000], 4123659995 / 4294967296.0, tolerance=2^-23)
})

test_that("bedrock_random_float works", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_float(10000)
    expect_equal(values[10000], 4123659995 / 4294967296.0, tolerance=2^-23)
})

test_that("bedrock_random_float works with a maximum value", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_float(10000, 10)
    expect_equal(values[10000], 9.60114383697509765625, tolerance=2^-23)
})

test_that("bedrock_random_float works with a minimum and a maximum value", {
    bedrock_random_seed(5489L)

    values <- bedrock_random_get_float(10000, 10, 20)
    expect_equal(values[10000], 19.60114288330078125, tolerance=2^-23)
})

test_that("bedrock_random_create_seed1 finds slime chunks", {

    is_slime_chunk <- function(x,z) {
        seed <- bedrock_random_create_seed(x, z, 0x1f1f1f1f, 1, 0, type=1)
        bedrock_random_seed(seed)
        bedrock_random_get_uint(1,10) == 0
    }

    expect_equal(is_slime_chunk(0,0), FALSE)
    expect_equal(is_slime_chunk(3,1), TRUE)
    expect_equal(is_slime_chunk(-61,-59), TRUE)
    expect_equal(is_slime_chunk(-61,-60), FALSE)
})
