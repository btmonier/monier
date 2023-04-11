# === Tests for `biocStats` function ==================================

## Error Tests ----
test_that("biocStats returns error when package is not available", {

    expect_error(
        biocStats("asdf")
    )

})


## Message Tests ----
test_that("biocStats returns query message", {

    expect_message(
        object = biocStats("vidger"),
        regexp = "Contacting server..."
    )

})

