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

    if (use < 0) stop("Usage must be >= 0.")
    if (total <= 0) stop("Total usage must be greater than 0.")
    if (charLen <= 0) stop("Character length must be greater than 0.")
    if (!is.numeric(use) || !is.numeric(total) || !is.numeric(charLen)) stop("Values must be numeric")
    if (use > total) stop("Usage exceeds total usage.")
    if (length(charEnd) > 2) warning("First two elements will be used.")
    if (length(charEnd) == 1) charEnd <- rep(charEnd, 2)


    propUse <- use / total
    propRem <- 1 - propUse

    numUse <- round(propUse * charLen, 0)

    # Small cases
    if (numUse == 0) {
        numUse <- 1
    } else if (numUse == charLen) {
        numUse <- charLen - 1
    }

    # Extreme cases
    if (use == 0) {
        numUse <- 0
    } else if (use == total) {
        numUse <- charLen
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
