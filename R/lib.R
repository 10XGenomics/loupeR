#' Create a Loupe file from a Seurat Object
#'
#' @description
#' `create_loupe_from_seurat()` passes the active counts matrix,
#' reductions, and factors found in `meta.data` to create a Loupe file.
#'
#' @param obj A Seurat Object
#' @param output_dir optional directory where the Loupe file will be written
#' @param output_name optional name of the Loupe file with the extensions not included.
#' @param metadata_cols optional list that specifies which metadata columns to retain when selecting clusters
#' @param dedup_clusters optional logical that will try to deduplicate all clusters that are numerically the same
#' @param feature_ids optional character vector that specifies the feature ids of the count matrix.
#'   Typically, these are the ensemble ids.
#' @param executable_path optional path to the louper executable.
#' @param force optional logical as to whether we should overwrite an already existing file
#'
#' @return TRUE on success, FALSE on error
#'
#' @importFrom methods is
#'
#' @export
create_loupe_from_seurat <- function(
    obj,
    output_dir = NULL,
    output_name = NULL,
    metadata_cols = NULL,
    dedup_clusters = FALSE,
    feature_ids = NULL,
    executable_path = NULL,
    force = FALSE) {
  v <- needs_setup(executable_path)
  if (!v$success) {
    stop(v$msg)
  }

  if (!is(obj, "Seurat")) {
    stop(validation_err("input object was not a Seurat object", "Seurat Object"))
  }

  log_msg("extracting matrix, clusters, and projections")

  named_assay <- select_assay(obj)
  if (is.null(named_assay)) {
    stop(validation_err("could not find a usable count matrix", "Seurat Object"))
  }

  assay_name <- names(named_assay)
  assay <- named_assay[[1]]
  counts <- counts_matrix_from_assay(assay)

  clusters <- select_clusters(obj, dedup = dedup_clusters, metadata_cols = metadata_cols)
  projections <- select_projections(obj)

  log_msg("selected assay:", assay_name)
  log_msg("selected clusters:", names(clusters))
  log_msg("selected projections:", names(projections))

  seurat_obj_version <- NULL
  if (!is.null(obj@version)) {
    seurat_obj_version <- as.character(obj@version)
  }

  success <- create_loupe(
    counts,
    clusters = clusters,
    projections = projections,
    output_dir = output_dir,
    output_name = output_name,
    feature_ids = feature_ids,
    executable_path = executable_path,
    force = force,
    seurat_obj_version = seurat_obj_version
  )

  invisible(success)
}

#' Create a Loupe file
#'
#' @param count_mat A sparse dgCMatrix as is generated via Matrix::rsparsematrix.
#'   Rows are features, Columns are barcodes.
#' @param clusters list of factors that hold information for each barcode
#' @param projections list of matrices, all with dimensions (barcodeCount x 2)
#' @param output_dir optional directory where the Loupe file will be written
#' @param output_name optional name of the Loupe file with the extensions not included.
#' @param feature_ids optional character vector that specifies the feature ids of the count matrix.
#'   Typically, these are the ensemble ids.
#' @param executable_path optional path to the louper executable.
#' @param force optional logical as to whether we should overwrite an already existing file
#' @param seurat_obj_version optional string that holds the Seurat Object version.
#'   It is useful for debugging compatibility issues.
#'
#' @return TRUE on success, FALSE on error
#'
#' @importFrom methods is
#'
#' @export
create_loupe <- function(
    count_mat,
    clusters = list(),
    projections = list(),
    output_dir = NULL,
    output_name = NULL,
    feature_ids = NULL,
    executable_path = NULL,
    force = FALSE,
    seurat_obj_version = NULL) {
  v <- needs_setup(executable_path)
  if (!v$success) {
    stop(v$msg)
  }

  log_msg("validating count matrix")
  ok <- validate_count_mat(count_mat, feature_ids = feature_ids)
  if (!ok$success) {
    stop(validation_err(ok$msg, "count matrix"))
  }

  barcodes <- colnames(count_mat)
  barcode_count <- length(barcodes)

  log_msg("validating clusters")
  ok <- validate_clusters(clusters, barcode_count)
  if (!ok$success) {
    stop(validation_err(ok$msg, "clusters"))
  }

  log_msg("validating projections")
  ok <- validate_projections(projections, barcode_count)
  if (!ok$success) {
    stop(validation_err(ok$msg, "projections"))
  }

  h5path <- sprintf("%s.h5", tempfile())
  log_msg("creating temporary hdf5 file:", h5path)
  ok <- create_hdf5(
    count_mat,
    clusters,
    projections,
    h5path,
    feature_ids,
    seurat_obj_version
  )
  if (!ok$success) {
    stop(general_err(ok$msg, "creating the temporary hdf5 file"))
  }

  log_msg("invoking louper executable")
  ok <- louper_create_cloupe(
    h5path,
    output_dir = output_dir,
    output_name = output_name,
    executable_path = executable_path,
    force = force
  )
  if (!ok$success) {
    stop(general_err(ok$msg, "creating the loupe file"))
  }

  invisible(TRUE)
}
