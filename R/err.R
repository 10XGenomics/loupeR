#' Generate an error with the given msg
#'
#' @param msg is a string
#'
#' @noRd
err <- function(msg) {
  list(success = FALSE, msg = msg)
}

#' The non-error variant of above
#' @noRd
SUCCESS <- list(success = TRUE, msg = NULL)

#' validation error with link to 10x support
#' @noRd
validation_err <- function(msg, name) {
  sprintf("\nIt looks like the formatting of your %s does not match the required formatting for LoupeR. For further information, please see the documentation: 10xgen.com/louper\n\n%s", name, msg)
}

#' general error with link to 10x support
#' @noRd
general_err <- function(msg, name) {
  sprintf("\nIt looks like there was an issue with %s. For further information, please see the documentation: 10xgen.com/louper\n\n%s", name, msg)
}

#' Create a Bugreport from a Seurat Object
#'
#' @description
#' This bugreport can then be included when reaching out to 10xGenomics Support or when filing
#' a Github ticket.  This information should be included along with any other output when creating a Loupe file.
#'
#' @param obj A Seurat Object
#'
#' @importFrom methods is
#'
#' @export
create_bugreport_from_seurat <- function(obj) {
  # metadata
  cat("\nMetadata:\n\n")
  if (is(obj, "Seurat")) {
    obj_version <- as.character(obj@version)
    metadata <- create_metadata(obj_version)
    print_metadata(metadata)
  } else {
    metadata <- create_metadata()
    print_metadata(metadata)
    print_lines(sprintf("Object is not a Seurat Object it is a: %s", class(obj)))
    return(invisible())
  }

  # overview
  namedAssay <- select_assay(obj)
  if (is.null(namedAssay)) {
    cat("\nSeurat:\n\n")
    cat("No assay found\n")
    return(invisible())
  }

  assay_name <- names(namedAssay)
  assay <- namedAssay[[1]]
  clusters <- select_clusters(obj)
  projections <- select_projections(obj)

  create_bugreport(
    assay@counts,
    clusters,
    projections,
    assay_name = assay_name,
    seurat_obj_version = obj_version,
    skip_metadata = TRUE
  )
}

#' Create a Bugreport from a count matrix, projections, and clusters
#'
#' @description
#' This bugreport can then be included when reaching out to 10xGenomics Support or when filing
#' a Github ticket.  This information should be included along with any other output when creating a Loupe file.
#'
#' @param count_mat A sparse dgCMatrix
#' @param clusters list of factors that hold information for each barcode
#' @param projections list of matrices, all with dimensions (barcodeCount x 2)
#' @param assay_name optional string that holds the Seurat Object assay name.
#' @param seurat_obj_version optional string that holds the Seurat Object version.  It is useful for debugging compatibility issues.
#' @param skip_metadata optional logical which skips printing metadata
#'
#' @importFrom methods is
#'
#' @export
create_bugreport <- function(
  count_mat,
  clusters,
  projections,
  assay_name = NULL,
  seurat_obj_version = NULL,
  skip_metadata = FALSE
) {
  # metadata
  if (!skip_metadata) {
    cat("\nMetadata:\n\n")
    metadata <- create_metadata(seurat_obj_version)
    print_metadata(metadata)
  }

  # selections
  cat("\nSelections:\n\n")
  if (!is.null(assay_name)) {
    cat("selected assay:\n")
    print_lines(assay_name, "    ")
  }
  cat("selected clusters:\n")
  print_lines(names(clusters), "    ")
  cat("selected projections:\n")
  print_lines(names(projections), "    ")

  # matrix
  cat("\nMatrix Sampling:\n\n")
  all_features <- rownames(count_mat)
  all_barcodes <- colnames(count_mat)
  features <- sample(rownames(count_mat), size=min(10, length(all_features)))
  barcodes <- sample(colnames(count_mat), size=min(10, length(all_barcodes)))
  cat(sprintf("feature count: %d\n", length(all_features)))
  cat(sprintf("barcode count: %d\n", length(all_barcodes)))
  cat(sprintf("feature sampling:\n"))
  print_lines(features, "    ")
  cat(sprintf("barcode sampling:\n"))
  print_lines(barcodes, "    ")

  print_validation <- function(name, res) {
    if (res$success) {
      cat(sprintf("    %s (VALID)\n", name))
    } else {
      cat(sprintf("    %s (INVALID) %s\n", name, res$msg))
    }
  }

  # validation
  cat("\nValidation:\n\n")
  print_validation("count matrix:", validate_count_mat(count_mat))
  print_validation("clusters:    ", validate_clusters(clusters, length(all_barcodes)))
  print_validation("projections: ", validate_projections(projections, length(all_barcodes)))
}
