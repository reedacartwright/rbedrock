dbpath <- rbedrock_example_world("example1.mcworld")
db <- bedrockdb(dbpath)

test_that("get_hsa_data can read HSA data.", {
    hsa <- get_hsa_data(db)
    expected_hsa <- tibble::tibble(
        tag = "PillagerOutpost",
        x1 = c(577, 577, 592, 592), y1 = 65, z1 = c(241, 256, 241, 256), 
        x2 = c(591, 591, 592, 592), y2 = 86, z2 = c(255, 256, 255, 256),
        xspot = c(584, 584, 592, 592),
        yspot = c(82, 82, 82, 82),
        zspot = c(248, 256, 248, 256),
        dimension = 0,
        key = c("@36:15:0:57", "@36:16:0:57", "@37:15:0:57", "@37:16:0:57")        
    )
    expect_equal(hsa, expected_hsa)
})

test_that("get_hsa_data returns an empty table if no HSA is found.", {
    hsa <- get_hsa_data(db, "@0:0:0:57")
    expected_hsa <- tibble::tibble(
        tag = character(0),
        x1 = integer(0), y1 = integer(0), z1 = integer(0), 
        x2 = integer(0), y2 = integer(0), z2 = integer(0),
        xspot = integer(0),
        yspot = integer(0),
        zspot = integer(0),
        dimension = integer(0),
        key = character(0)
    )
    expect_equal(hsa, expected_hsa)
})

test_that("get_hsa_value can read a single HSA data.", {
    hsa <- get_hsa_value(db, 36, 15, 0)

    expected_hsa <- tibble::tibble(
        tag = "PillagerOutpost",
        x1 = 577, y1 = 65, z1 = 241, 
        x2 = 591, y2 = 86, z2 = 255,
        xspot = 584,
        yspot = 82,
        zspot = 248,
        dimension = 0,
        key = "@36:15:0:57"
    )
    expect_equal(hsa, expected_hsa)

    expect_error(get_hsa_value(db, c("@36:15:0:57", "@36:16:0:57")))
})

test_that("get_hsa_value returns an empty table if no HSA is found.", {
    hsa <- get_hsa_value(db, "@0:0:0:57")
    expected_hsa <- tibble::tibble(
        tag = character(0),
        x1 = integer(0), y1 = integer(0), z1 = integer(0), 
        x2 = integer(0), y2 = integer(0), z2 = integer(0),
        xspot = integer(0),
        yspot = integer(0),
        zspot = integer(0),
        dimension = integer(0),
        key = character(0)
    )
    expect_equal(hsa, expected_hsa)
})

test_that("put_hsa_data can write HSA data.", {
    dat <- data.frame(x1 = 295, x2 = 300, z1 = 100, z2 = 110, y1 = 64, y2=70, tag = 2L, dimension=0L)
    hsa <- put_hsa_data(db, dat, merge = FALSE)
    expected_hsa <- tibble::tibble(
        tag = "SwampHut",
        x1 = 295L, y1 = 64L, z1 = 100L, 
        x2 = 300L, y2 = 70L, z2 = 110L,
        key = "@18:6:0:57"
    )
    expect_equal(hsa, expected_hsa)

    expected_hsa <- tibble::tibble(
        tag = "SwampHut",
        x1 = 295L, y1 = 64L, z1 = 100L, 
        x2 = 300L, y2 = 70L, z2 = 110L,
        xspot = (295+300+1) %/% 2,
        yspot = 70 - 4,
        zspot = (110+100+1) %/% 2,
        dimension = 0L,
        key = "@18:6:0:57"
    )

    new_hsa <- get_hsa_data(db, "@18:6:0:57")
    expect_equal(new_hsa, expected_hsa)

    dat1 <- data.frame(x1 = 0, x2 = 15, z1 = 0, z2 = 15, y1 = 64, y2 = 70,
        tag = "NetherFortress")
    dat2 <- data.frame(x1 = 0, x2 = 16, z1 = 0, z2 = 15, y1 = 64, y2 = 70,
        tag = "SwampHut")
    dat <- dplyr::bind_rows(dat1, dat2)
    hsa <- put_hsa_data(db, dat, merge = FALSE)

    expected_hsa <- tibble::tibble(
        tag = c("NetherFortress", "SwampHut", "SwampHut"),
        x1 = c(0,   0, 16), y1 = 64, z1 = c(0, 0, 0), 
        x2 = c(15, 15, 16), y2 = 70, z2 = c(15, 15, 15),
        xspot = c(8, 8, 16),
        yspot = c(69, 66, 66),
        zspot = c(8, 8, 8),
        dimension = c(1, 0, 0),
        key = c("@0:0:1:57", "@0:0:0:57", "@1:0:0:57")
    )

    new_hsa <- get_hsa_data(db, c("@0:0:1:57", "@0:0:0:57", "@1:0:0:57"))
    expect_equal(new_hsa, expected_hsa)
})

test_that("put_hsa_data can merge HSA data.", {
    dat <- data.frame(
        x1 = 295, x2 = 300,
        z1 = 100, z2 = 120,
        y1 = 64, y2=70,
        tag = 3L, dimension = 0L)
    hsa <- put_hsa_data(db, dat)

    expected_hsa <- tibble::tibble(
        tag = c("OceanMonument","OceanMonument"),
        x1 = c(295, 295), y1 = c(64, 64), z1 = c(100, 112), 
        x2 = c(300, 300), y2 = c(70, 70), z2 = c(111, 120),
        key = c("@18:6:0:57", "@18:7:0:57")
    )

    expect_equal(hsa, expected_hsa)

    new_hsa <- get_hsa_data(db, c("@18:6:0:57","@18:7:0:57"))

    expected_hsa <- tibble::tibble(
        tag = c("SwampHut", "OceanMonument", "OceanMonument"),
        x1 = c(295, 295, 295), y1 = c(64, 64, 64), z1 = c(100, 100, 112), 
        x2 = c(300, 300, 300), y2 = c(70, 70, 70), z2 = c(110, 111, 120),
        xspot = c(298, 298, 298),
        yspot = c(66, 69, 69),
        zspot = c(105, 106, 116),
        dimension = c(0,0,0),
        key = c("@18:6:0:57", "@18:6:0:57", "@18:7:0:57")        
    )

    expect_equal(new_hsa, expected_hsa)
})

test_that("put_hsa_value can write HSA data.", {
    dat <- data.frame(
        x1 = 0, x2 = 15,
        z1 = 0, z2 = 15,
        y1 = 100, y2 = 110,
        tag = "SwampHut", dimension = 2)
    put_hsa_value(db, 0, 0, 2, dat)

    hsa <- get_hsa_value(db, "@0:0:2:57")

    expected_hsa <- tibble::tibble(
        tag = "SwampHut",
        x1 = 0, y1 = 100, z1 = 0, 
        x2 = 15, y2 = 110, z2 = 15,
        xspot = 8,
        yspot = 106,
        zspot = 8,
        dimension = 2,
        key = "@0:0:2:57"
    )
    expect_equal(hsa, expected_hsa)
})

test_that("put_hsa_values can write HSA data.", {

    dat1 <- data.frame(x1 = 0, x2 = 15, z1 = 0, z2 = 15, y1 = 40, y2 = 60,
        tag = 1)
    dat2 <- data.frame(x1 = 0, x2 = 15, z1 = 0, z2 = 15, y1 = 40, y2 = 60,
        tag = 2)

    dat <- list(dat1, dat2)

    put_hsa_values(db, c("@0:0:1:57", "@0:0:0:57"), values=dat)

    hsa <- get_hsa_data(db, c("@0:0:1:57", "@0:0:0:57"))

    expected_hsa <- tibble::tibble(
        tag = c("NetherFortress", "SwampHut"),
        x1 = 0, y1 = 40, z1 = 0, 
        x2 = 15, y2 = 60, z2 = 15,
        xspot = 8,
        yspot = c(59,56),
        zspot = 8,
        dimension = c(1,0),
        key = c("@0:0:1:57", "@0:0:0:57")
    )
    expect_equal(hsa, expected_hsa)
})

# clean up
close(db)
fs::dir_delete(dbpath)
