#' @title Be right back message prompt
#'
#' @param t Time
#' @param unit Unit of time `t`. Defaults to minute (`m`).
#'
#' @export
brb <- function(t, unit = c("m", "s", "h")) {
    timeSnap <- Sys.time()

    unit <- match.arg(unit)

    unitConv <- switch (
        EXPR = unit,
        "m"  = 60,
        "s"  = 1,
        "h"  = 3600
    )

    timeSnapReturn <- timeSnap + (t * unitConv)

    message("We'll be back at: ", timeSnapReturn)
}