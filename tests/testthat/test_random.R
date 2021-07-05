test_that("default mcpe_random seed is 5489", {
    state_default <- mcpe_random_state()

    mcpe_random_seed(5489L)
    state_5489 <- mcpe_random_state()

    expect_equal(state_default, state_5489)
})

test_that("the 10,000th value of the default-constructed mcpe_random is 4123659995", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_uint(10000)
    expect_equal(values[10000], 4123659995)
})

test_that("mcpe_random_state() restores state", {
    mcpe_random_seed(5490L)

    saved_state <- mcpe_random_state()
    values1 <- mcpe_random_get_uint(10000)
    state1 <- mcpe_random_state(saved_state)

    values2 <- mcpe_random_get_uint(10000)
    state2 <- mcpe_random_state()

    expect_equal(values1, values2)
    expect_equal(state1, state2)
})

test_that("mcpe_random_uint32 works", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_uint(10000)
    expect_equal(values[10000], 4123659995)
})

test_that("mcpe_random_uint32 works with a maximum value", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_uint(10000, 17)
    expect_equal(values[10000], 4123659995 %% 17)
})

test_that("mcpe_random_int32 works", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_int(10000)
    expect_equal(values[10000], 4123659995 %/% 2)
})

test_that("mcpe_random_int32 works with a maximum value", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_int(10000, 17)
    expect_equal(values[10000], 4123659995 %% 17)

    values <- mcpe_random_get_int(10,0)
    expect_equal(values, rep(0,10L))
})

test_that("mcpe_random_int32 works with a minimum and a maximum value", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_int(10000, 10, 17)
    expect_equal(values[10000], (4123659995 %% 7) + 10)

    values <- mcpe_random_get_int(10,20,20)
    expect_equal(values, rep(20,10L))

    values <- mcpe_random_get_int(10,20,10)
    expect_equal(values, rep(20,10L))
})

test_that("mcpe_random_double works", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_double(10000)
    expect_equal(values[10000], 4123659995 / 4294967296.0)
})

test_that("mcpe_random_float works", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_float(10000)
    expect_equal(values[10000], 4123659995 / 4294967296.0)
})

test_that("mcpe_random_float works with a maximum value", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_float(10000, 10)
    expect_equal(values[10000], 9.60114383697509765625)
})

test_that("mcpe_random_float works with a minimum and a maximum value", {
    mcpe_random_seed(5489L)

    values <- mcpe_random_get_float(10000, 10, 20)
    expect_equal(values[10000], 19.60114288330078125)
})
