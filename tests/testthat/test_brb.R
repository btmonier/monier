# === Tests for `brb` function ======================================

#####################################################################
## Return Tests ----
test_that("", {
    expect_message(brb(5, "m"))
    expect_message(brb(5, "h"))
    expect_message(brb(5, "s"))
    expect_error(brb(5, "x"))
})
