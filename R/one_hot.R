## ----
#' @title One-hot encode a DNA sequence
#'
#' @description Takes a DNA string and returns a one-hot encoded matrix of
#'    4 rows \code{c("A", "C", "G", "T")} with \eqn{j}-columns, where
#'    \eqn{j} is the number of characters in a DNA string.
#'
#' @param s A character string of a DNA sequence.
#' @param n A character vector of given nucleotide elements. Defaults to
#'    \code{c("A", "C", "G", "T")}.
#'
#' @export
oneHot <- function(s, n = c("A", "C", "G", "T")) {

    # Construct matrix
    s <- toupper(s)
    seq_len <- nchar(s)
    seq_split <- unlist(strsplit(x = s, split = ""))

    seq_mat <- matrix(data = rep(0, seq_len * length(n)), nrow = 4)

    rownames(seq_mat) <- n
    colnames(seq_mat) <- seq_split

    # Encode
    for (i in n) {
        seq_mat[rownames(seq_mat) == i, colnames(seq_mat) == i] <- 1
    }

    return(seq_mat)

}


