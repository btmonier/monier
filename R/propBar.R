#' @title Make text-based proportion bars in the console
#'
#' @description Generates a text-based proportion bar for downstream
#'    applications.
#'
#' @param use Number of units used in total size.
#' @param total Total number of units in total size.
#' @param charUse Text character to display used portion of bar. Defaults to
#'    \code{"#"}.
#' @param charRem Text character to display unused portion of bar. Defaults to
#'    \code{" "}.
#' @param charLen Total number of characters used for bar. Default is 25 units.
#' @param charEnd "Fancy" end pieces if you want them. Must be supplied as a
#'    vector of character elements. Defaults to \code{c("[", "]")}.
#'
#' @export
propBar <- function(use,
                    total,
                    charUse = "#",
                    charRem = " ",
                    charLen = 25,
                    charEnd = c("[", "]")) {
    propUse <- use / total
    propRem <- 1 - propUse

    numUse <- round(propUse * charLen, 0)

    if (use == 0) {
        numUse <- 0
    } else if (numUse == 0) {
        numUse <- 1
    } else if (numUse == charLen) {
        numUse <- charLen - 1
    } else {
        numUse
    }

    numRem <- charLen - numUse

    text_bar <- paste0(
        charEnd[1],
        paste(rep(charUse, numUse), collapse = ""),
        paste(rep(charRem, numRem), collapse = ""),
        charEnd[2]
    )

    return(text_bar)
}