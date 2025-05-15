#' Create random barcode
random_barcode <- function(size = 14) {
  paste0(sample(c("A", "C", "T", "G"), size, replace = TRUE), collapse = "")
}

#' Create a sparse count_mat
#'
#' @importFrom Matrix rsparsematrix
create_count_mat <- function(rows, cols, valid_barcodes = FALSE) {
  mat <- Matrix::rsparsematrix(rows, cols, 0.5, rand.x = function(n) as.integer(100 * runif(n)))

  rownames <- as.character()
  if (rows > 0) {
    rownames <- paste0("row", 1:rows)
  }

  colnames <- as.character()
  if (cols > 0) {
    if (valid_barcodes) {
      colnames <- lapply(rep(14, cols), random_barcode)
    } else {
      colnames <- paste0("col", 1:cols)
    }
  }

  dimnames(mat) <- list(rownames, colnames)
  mat
}

#' Create a dimensional reduction (projection) object
create_dim_reduction <- function(count_mat, key) {
  barcode_count <- dim(count_mat)[2]

  proj <- create_dense_mat(barcode_count, 2)

  rownames(proj) <- colnames(count_mat)
  colnames(proj) <- c(paste0(key, 1), paste0(key, 2))

  Seurat::CreateDimReducObject(
    embeddings = proj,
    key = paste0(key, "_"),
    assay = "rna",
    global = TRUE
  )
}

#' Create a dense matrix
create_dense_mat <- function(rows, cols) {
  count <- rows * cols
  matrix(runif(count), nrow = rows)
}

get_executable_path <- function() {
  wd <- getwd()
  os <- get_system_os()
  if (os == "windows") {
    file.path(wd, "mock_louper.bat")
  } else {
    file.path(wd, "mock_louper")
  }
}
