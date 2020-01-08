# === Tests for `peak` function =====================================

## Make test matrix ----
test_mat <- matrix(data = rep(0, 25), nrow = 5, ncol = 5)


## Tests ----
test_that("peak function returns error", {

    # Rows
    expect_error(
        peak(x = test_mat, pi = 6, pj = 4)
    )

    # Columns
    expect_error(
        peak(x = test_mat, pi = 3, pj = 7)
    )

})


test_that("peak function returns correct dimensions", {

    expect_equal(
        object = dim(peak(x = test_mat, pi = 3, pj = 3)),
        expected = c(3, 3)
    )

})
