## ----
#' Create Subdirectories
#'
#' This function creates a set of standard subdirectories ("data",
#' "src", "output", "notebook") in the current working directory
#' or in a specified parent directory.
#'
#' @param p A character string specifying the path to the parent
#' directory where subdirectories should be created. If `NULL`,
#' the subdirectories will be created in the current working
#' directory. Default is `NULL`.
#'
#' @return The function creates directories as a side effect
#' and returns `NULL` invisibly.
#'
#' @examples
#' # Create directories in the current working directory
#' createDirs()
#'
#' # Create directories inside an existing parent directory
#' createDirs("project_folder")
#'
#' @seealso \code{\link{dir.create}}, \code{\link{file.path}}
#'
#' @export
createDirs <- function(p = NULL) {
    subDirs <- c("data", "src", "output", "notebook")

    if (!is.null(p)) {
        if (!dir.exists(p)) {
            stop("Parent directory ('p') does not exist")
        }

        subDirs <- file.path(p, subDirs)
    }

    for (i in subDirs) {
        if (!dir.exists(i)) {
            dir.create(i)
        }
    }
}


