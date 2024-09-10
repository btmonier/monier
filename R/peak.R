## ----
#' @title Get snapshot of large data objects
#'
#' @description Reveals first i rows and j columns.
#'
#' @param x An R data frame or matrix object.
#' @param pi How many rows do you want to select? Defaults to \code{10}.
#' @param pj How many columns do you want to select? Defaults to \code{4}.
#'
#' @export
peak <- function(x, pi = 10, pj = 4) {

    if (pi > dim(x)[1] || pj > dim(x)[2]) {
        stop("More rows were selected than provided.")
    }

    return(x[1:pi, 1:pj])
}


