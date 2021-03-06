# === Tests for `propBar` function ==================================

#####################################################################
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



#####################################################################
## Warning Tests ----
test_that("propBar returns warning when endcaps do not equal 2", {

    expect_warning(
        propBar(use = 5, total = 10, charEnd = c("[", "]", "|"))
    )

})



#####################################################################
## Return Tests ----
test_that("propBar function can return default output", {

    expect_identical(
        object = propBar(use = 5, total = 10, charLen = 25),
        expected = "[############             ]"
    )

})

test_that("propBar can return custom characters", {

    expect_identical(
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
    expect_identical(
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

    expect_identical(
        object = propBar(use = 0, total = 10, charLen = 25),
        expected = "[                         ]"
    )

})

test_that("propBar returns a full bar", {

    expect_identical(
        object = propBar(use = 10, total = 10, charLen = 25),
        expected = "[#########################]"
    )

})

test_that("propBar returns units if usage is very small", {

    expect_identical(
        object = propBar(use = 0.01, total = 10, charLen = 25),
        expected = "[#                        ]"
    )

})

test_that("propBar returns units if usage is very close to total", {

    expect_identical(
        object = propBar(use = 9.99, total = 10, charLen = 25),
        expected = "[######################## ]"
    )

})


test_that("propBar returns end caps if only one is entered", {

    expect_identical(
        object = propBar(use = 5, total = 10, charLen = 25, charEnd = "|"),
        expected = "|############             |"
    )

})


