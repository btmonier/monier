#' @title Get snapshot of large data objects
#'
#' @description Reveals first i rows and j columns.
#'
#' @param x An R data frame or matrix object
#'
#' @export
peak <- function(x, pi = 10, pj = 4) {
    return(x[1:pi, 1:pj])
}
