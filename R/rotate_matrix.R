## ----
#' @title Rotate matrices
#'
#' @description Rotates matrices to corresponding console output.
#'
#' @param x An R matrix object.
#'
#' @export
rotateMatrix <- function(x) {
    if (!is.matrix(x)) stop("Object must be of matrix class.")

    t(apply(x, 2, rev))
}


