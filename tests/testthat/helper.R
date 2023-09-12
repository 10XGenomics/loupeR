#' Create a sparse count_mat
#'
#' @importFrom Matrix rsparsematrix
create_count_mat <- function(rows, cols) {
  mat <- Matrix::rsparsematrix(rows, cols, 0.5, rand.x = function(n) as.integer(100*runif(n)))

  rownames <- as.character()
  if (rows > 0) {
    rownames <- paste0("row", 1:rows)
  }

  colnames <- as.character()
  if (cols > 0) {
    colnames <- paste0("col", 1:cols)
  }

  dimnames(mat) <- list(rownames, colnames)
  mat
}

#' Create a dense matrix
create_dense_mat <- function(rows, cols) {
  count <- rows * cols
  matrix(runif(count), nrow=rows)
}
