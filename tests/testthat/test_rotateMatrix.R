# === Tests for `rotateMatrix` function =============================

## Error Tests ----
test_that("rotatMatrix returns error when object is not of matrix class", {

    wrong <- data.frame(a = 1:3, b = letters[1:3])

    expect_error(
        rotatMatrix(x = wrong)
    )

})


## Return Tests ----
test_that("rotateMatrix can return default output", {

    testMat <- matrix(data = c(1, rep(0, 8)), nrow = 3, ncol = 3)
    expMat  <- matrix(data = c(0, 0, 1, rep(0, 6)), nrow = 3, byrow = TRUE)

    expect_equal(
        object = rotateMatrix(x = testMat),
        expected = expMat
    )

})

