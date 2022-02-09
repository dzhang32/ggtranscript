# create dummy exons for testing
test_exons <-
    dplyr::tibble(
        start = c(100, 300, 500, 650),
        end = start + 100,
        strand = c("+", "+", "-", "-"),
        tx = c("A", "A", "B", "B")
    )

##### .check_coord_object #####

testthat::test_that(".check_coord_object() works correctly", {
    expect_equal(
        .check_coord_object(test_exons),
        NULL
    )
})

testthat::test_that(".check_coord_object() catches user input errors", {
    expect_error(
        .check_coord_object("1"),
        "must be a data.frame"
    )
    expect_error(
        .check_coord_object(test_exons %>% dplyr::select(-start)),
        "must have the columns"
    )
    expect_error(
        .check_coord_object(test_exons %>% dplyr::select(-end)),
        "must have the columns"
    )
    expect_error(
        .check_coord_object(test_exons, check_seqnames = TRUE),
        "must have the column"
    )
    expect_error(
        .check_coord_object(
            test_exons %>% dplyr::select(-strand),
            check_strand = TRUE
        ),
        "must have the column"
    )
})

##### .check_group_var #####

testthat::test_that(".check_group_var() works correctly", {
    expect_equal(
        .check_group_var(test_exons, group_var = NULL),
        NULL
    )
    expect_equal(
        .check_group_var(test_exons, group_var = "tx"),
        NULL
    )
})

testthat::test_that(".check_group_var() catches user input errors", {
    expect_error(
        .check_group_var(test_exons, "not_a_col"),
        "must be a column in"
    )
})
