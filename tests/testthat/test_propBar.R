# === Tests for `propBar` function ==================================

## Tests ----
test_that("propBar function returns error", {

    # Usage exceeds total values
    expect_error(
        propBar(use = 11, total = 10)
    )

    # Values are not numeric
    expect_error(
        propBar(use = "10", total = 10)
    )
    expect_error(
        propBar(use = 10, total = "10")
    )
    expect_error(
        propBar(use = 10, total = 10, charLen = "25")
    )

    # End caps do not equal 2
    expect_error(
        propBar(use = 10, total = 10, charEnd = c("["))
    )

})


test_that("propBar function returns correct output", {

    # Normal conditions
    expect_equal(
        object = propBar(use = 5, total = 10, charLen = 25),
        expected = "[############             ]"
    )

    # Normal conditions - change character types
    expect_equal(
        object = propBar(
            use     = 5,
            total   = 10,
            charLen = 25,
            charUse = "=",
            charRem = "-"
        ),
        expected = "[============-------------]"
    )

    # Normal conditions - change character types
    expect_equal(
        object = propBar(
            use     = 5,
            total   = 10,
            charLen = 25,
            charEnd = c("|", "|")
        ),
        expected = "|############             |"
    )

    # What if usage is 0?
    expect_equal(
        object = propBar(use = 0, total = 10, charLen = 25),
        expected = "[                         ]"
    )

    # What if usage == total?
    expect_equal(
        object = propBar(use = 10, total = 10, charLen = 25),
        expected = "[#########################]"
    )

})


