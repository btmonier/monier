# === Tests for `propBar` function ==================================

## Error Tests ----
test_that("propBar returns error when usage exceeds total", {

    # Usage exceeds total values
    expect_error(
        propBar(use = 11, total = 10)
    )

})

test_that("propBar returns error when non-numeric entities are used", {

    expect_error(
        propBar(use = "10", total = 10)
    )
    expect_error(
        propBar(use = 10, total = "10")
    )
    expect_error(
        propBar(use = 10, total = 10, charLen = "25")
    )

})

test_that("propBar returns error when endcaps do not equal 2", {

    expect_error(
        propBar(use = 10, total = 10, charEnd = c("["))
    )

})

test_that("propBar returns error when bar width is <= 0", {

    expect_error(
        propBar(use = 3, total = 10, charLen = -3)
    )

})

test_that("propBar returns error when total usage is <= 0", {

    expect_error(
        propBar(use = 3, total = -10)
    )

})

test_that("propBar returns error when usage is < 0", {

    expect_error(
        propBar(use = -3, total = 10)
    )

})

## Return Tests ----
test_that("propBar function can return default output", {

    expect_equal(
        object = propBar(use = 5, total = 10, charLen = 25),
        expected = "[############             ]"
    )

})

test_that("propBar can return custom characters", {

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

})

test_that("propBar can return custom endcaps", {
    expect_equal(
        object = propBar(
            use     = 5,
            total   = 10,
            charLen = 25,
            charEnd = c("|", "|")
        ),
        expected = "|############             |"
    )
})

test_that("propBar returns an empty bar", {

    expect_equal(
        object = propBar(use = 0, total = 10, charLen = 25),
        expected = "[                         ]"
    )

})

test_that("propBar returns a full bar", {

    expect_equal(
        object = propBar(use = 10, total = 10, charLen = 25),
        expected = "[#########################]"
    )

})



