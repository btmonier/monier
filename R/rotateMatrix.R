#' @title Rotate matrices
#'
#' @description Rotates matrices to corresponding console output.
#'
#' @param x An R matrix object.
#'
#' @export
rotateMatrix <- function(x) {
    t(apply(x, 2, rev))
}