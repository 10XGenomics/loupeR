#' Validate the seurat count matrix
#'
#' @param count_mat A sparse dgCMatrix as is generated via Matrix::rsparsematrix.
#'   Rows are features, Columns are barcodes.
#' @param feature_ids optional character vector that specifies the feature ids of the count matrix.
#'   Typically, these are the ensemble ids.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item success: a logical value indicating success (TRUE) or failure (FALSE)
#'   \item msg: an optional error message (NULL if success is TRUE)
#' }
#'
#' @importFrom methods is
#'
#' @export
validate_count_mat <- function(count_mat, feature_ids = NULL) { # nolint: cyclocomp_linter.
  if (!is(count_mat, "dgCMatrix")) {
    return(err("count_mat must be a dgCMatrix"))
  }

  features <- rownames(count_mat)
  barcodes <- colnames(count_mat)

  if (is.null(features)) {
    return(err("must supply feature dimnames on count_mat"))
  }
  if (is.null(barcodes)) {
    return(err("must supply barcodes dimnames on count_mat"))
  }
  if (length(features) == 0) {
    return(err("count_mat must have at least one feature"))
  }
  if (length(barcodes) == 0) {
    return(err("count_mat must have at least one barcode"))
  }
  if (any(is.nan(count_mat@x))) {
    return(err("matrix values must not be NaN"))
  }
  if (any(is.infinite(count_mat@x))) {
    return(err("matrix values must not be infinite"))
  }
  if (!all(sapply(barcodes, nzchar))) {
    return(err("barcodes cannot be the empty string"))
  }
  if (!all(sapply(features, nzchar))) {
    return(err("features cannot be the empty string"))
  }

  resp <- validate_barcodes(barcodes)
  if (!resp$success) {
    barcode_msg <- paste(
      "There is an issue with the formatting of your barcodes:",
      resp$msg,
      "Please see the readme at github.com/10xGenomics/loupeR"
    )

    return(err(barcode_msg))
  }

  if (length(unique(barcodes)) != length(barcodes)) {
    return(err("all barcodes should be unique"))
  }

  # validate feature ids
  if (!is.null(feature_ids)) {
    if (length(feature_ids) != length(features)) {
      return(err("must supply a feature id for each feature name"))
    }
    if (length(unique(feature_ids)) != length(feature_ids)) {
      return(err("all feature ids should be unique"))
    }
    if (!all(sapply(feature_ids, nzchar))) {
      return(err("feature ids cannot be the empty string"))
    }
  }

  SUCCESS
}

#' Validate the format of the barcodes
#'
#' @param barcodes a character vector
#'
#' @return A list with two elements:
#' \itemize{
#'   \item success: a logical value indicating success (TRUE) or failure (FALSE)
#'   \item msg: an optional error message (NULL if success is TRUE)
#' }
#'
#' @importFrom methods is
#'
#' @export
validate_barcodes <- function(barcodes) { # nolint: cyclocomp_linter.
  barcode_regex <- "^(.*[:_])?([ACGT]{14,})([:_].*)?$"
  barcode_gem_regex <- "^(.*[:_])?([ACGT]{14,})-(\\d+)([:_].*)?$"
  visium_hd_regex <- "^(.*[:_])?(s_\\d{3}um_\\d{5}_\\d{5})([:_].*)?$"
  visium_hd_gem_regex <- "^(.*[:_])?(s_\\d{3}um_\\d{5}_\\d{5})-(\\d+)([:_].*)?$"
  xenium_cell_id_regex <- "^(.*[:_])?([a-p]{1,8})-(\\d+)([:_].*)?$"

  for (barcode in barcodes) {
    if (
      !grepl(barcode_regex, barcode) &&
        !grepl(barcode_gem_regex, barcode) &&
        !grepl(visium_hd_regex, barcode) &&
        !grepl(visium_hd_gem_regex, barcode) &&
        !grepl(xenium_cell_id_regex, barcode)
    ) {
      return(err(paste("Invalid barcode:", barcode)))
    }
  }

  SUCCESS
}

#' Validate the seurat clusters
#'
#' @param clusters list of factors that hold information for each barcode
#' @param barcode_count number of barcodes
#'
#' @return A list with two elements:
#' \itemize{
#'   \item success: a logical value indicating success (TRUE) or failure (FALSE)
#'   \item msg: an optional error message (NULL if success is TRUE)
#' }
#'
#' @importFrom methods is
#'
#' @export
validate_clusters <- function(clusters, barcode_count) { # nolint: cyclocomp_linter.
  cluster_names <- names(clusters)

  if (!is.list(clusters)) {
    return(err("clusters must be in a list"))
  }
  if (length(clusters) == 0) {
    return(err("clusters must have at least one cluster"))
  }
  if (!all(sapply(clusters, is.factor))) {
    return(err("clusters must all be factors"))
  }
  if (is.null(cluster_names)) {
    return(err("clusters must supply names"))
  }
  if (!all(sapply(cluster_names, nzchar))) {
    return(err("cluster names cannot be the empty string"))
  }
  if (any(sapply(clusters, length) != barcode_count)) {
    return(err("cluster must have the same length as the number of barcodes"))
  }
  if (any(sapply(clusters, nlevels) > 32768)) {
    return(err("cluster cannot have more than 32768 groupings"))
  }
  if (any(sapply(clusters, nlevels) == 0)) {
    return(err("cluster must have at least one grouping"))
  }
  for (clusterIdx in seq_along(clusters)) {
    l <- levels(clusters[[clusterIdx]])
    if (!all(sapply(l, nchar))) {
      return(err("cluster group names cannot be the empty string"))
    }
  }

  SUCCESS
}

#' Validate the seurat projections
#'
#' @param projections list of matrices, all with dimensions (barcodeCount x 2)
#' @param barcode_count number of barcodes
#'
#' @return A list with two elements:
#' \itemize{
#'   \item success: a logical value indicating success (TRUE) or failure (FALSE)
#'   \item msg: an optional error message (NULL if success is TRUE)
#' }
#'
#' @importFrom methods is
#'
#' @export
validate_projections <- function(projections, barcode_count) { # nolint: cyclocomp_linter.
  is_projection <- function(p) {
    is.matrix(p)
  }

  # should have dimensions barcodeCount x 2
  projection_dims_good <- function(p) {
    pdims <- dim(p)
    pdims[[1]] == barcode_count && pdims[[2]] == 2
  }

  # no values should be NaN or Infinite
  projection_values_good <- function(p) {
    !any(is.nan(p)) &&
      !any(is.infinite(p))
  }

  proj_names <- names(projections)

  if (!is.list(projections)) {
    return(err("projections must be in a list"))
  }
  if (length(projections) == 0) {
    return(err("projections must have at least one projection"))
  }
  if (!all(sapply(projections, is_projection))) {
    return(err("projections must all be matrices"))
  }
  if (is.null(proj_names)) {
    return(err("projections must supply names"))
  }
  if (!all(sapply(proj_names, nzchar))) {
    return(err("projection names cannot be the empty string"))
  }
  if (!all(sapply(projections, projection_dims_good))) {
    return(err("projections must all have dimensions (BARCODE_COUNT, 2)"))
  }
  if (!all(sapply(projections, projection_values_good))) {
    return(err("projections must not contain NaN or infinite values"))
  }

  SUCCESS
}
