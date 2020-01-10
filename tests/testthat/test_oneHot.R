# === Tests for `oneHot` function ===================================

#####################################################################
## Return Tests ----
test_that("oneHot returns expected matrix", {

    # Make expected matrix
    expMat <- matrix(data = c(1, 0, 0, 0, 0, 0, 0, 1), nrow = 4)
    rownames(expMat) <- c("A", "C", "G", "T")
    colnames(expMat) <- c("A", "T")

    # Test
    expect_identical(
        object = oneHot(s = "AT"),
        expected = expMat
    )

})
