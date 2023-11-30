#' Create an hdf5 interchange file
#'
#' @param count_mat A sparse dgCMatrix
#' @param clusters list of factors that hold information for each barcode
#' @param projections list of matrices, all with dimensions (barcodeCount x 2)
#' @param h5path path to h5 file
#' @param feature_ids optional character vector that specifies the feature ids of the count matrix.  Typically, these are the ensemble ids.
#' @param seurat_obj_version optional string that holds the Seurat Object version.  It is useful for debugging compatibility issues.
#'
#' @importFrom hdf5r H5File
#'
#' @return TRUE on success, FALSE on error
#'
#' @noRd
create_hdf5 <- function(
  count_mat,
  clusters,
  projections,
  h5path,
  feature_ids,
  seurat_obj_version
) {
  if (file.exists(h5path)) {
    return(err(sprintf("cannot create h5 file %s", h5path)))
  }

  # create hdf5 file and matrix groups
  f <- hdf5r::H5File$new(h5path, mode="w")

  write_mat(f, count_mat, feature_ids)
  write_clusters(f, clusters)
  write_projections(f, projections)
  write_metadata(f, seurat_obj_version)

  f$close()

  SUCCESS
}

#' Writes the matrix to the H5 file
#'
#' @param f An open H5File
#' @param count_mat A sparse dgCMatrix
#' @param feature_ids optional character vector that specifies the feature ids of the count matrix.  Typically, these are the ensemble ids.
#'
#' @noRd
write_mat <- function(f, count_mat, feature_ids) {
  features <- rownames(count_mat)
  barcodes_unmodified <- colnames(count_mat)
  barcodes_formatted  <- sanitize_barcodes(barcodes_unmodified)
  feature_count <- length(features)
  barcode_count <- length(barcodes_formatted )

  # create groups
  matrix_group <- f$create_group("matrix")
  features_group <- matrix_group$create_group("features")

  create_str_dataset(matrix_group, "barcodes", barcodes_formatted )
  create_str_dataset(matrix_group, "barcodes_unmodified", barcodes_unmodified)
  create_dataset(matrix_group, "data", as.integer(count_mat@x))
  create_dataset(matrix_group, "indices", as.integer(count_mat@i))
  create_dataset(matrix_group, "indptr", as.integer(count_mat@p))
  create_dataset(matrix_group, "shape", as.integer(c(feature_count, barcode_count)))
  matrix_group$close()

  if (is.null(feature_ids)) {
    feature_ids <- lapply(1:length(features), function(x) {return(sprintf("feature_%d", x))})
  }

  create_str_dataset(features_group, "name", features)
  create_str_dataset(features_group, "id", as.character(feature_ids)) 
  create_str_dataset(features_group, "feature_type", rep("Gene Expression", length(features)))
  create_str_dataset(features_group, "_all_tag_keys", as.character()) # required features
  
  features_group$close()
}

#' Prints the metadata list to stdout.
#'
#' @param metadata The metadata list
#' @param prefix What to prefix each line
#'
#' @noRd
print_metadata <- function(metadata, prefix="") {
  for (name in names(metadata)) {
    val <- metadata[[name]]

    if (is.list(val)) {
      cat(sprintf("%s%s:\n", prefix, name))
      print_metadata(val, paste(prefix, "    "))
    } else {
      cat(sprintf("%s%s: %s\n", prefix, name, val))
    }
  }
}

#' Writes the clusters to the H5 file
#'
#' @param f An open H5File
#' @param clusters list of factors that hold information for each barcode
#'
#' @noRd
write_clusters <- function(f, clusters) {
  clusters_group <- f$create_group("clusters")

  for (i in seq_along(clusters)) {
    name <- names(clusters[i])
    cluster <- clusters[[i]]

    group <- clusters_group$create_group(name)
    create_str_dataset(group, "name", name)
    create_str_dataset(group, "group_names", levels(cluster))
    create_dataset(group, "assignments", as.integer(cluster@.Data - 1)) # zero index, so subtract 1
    create_dataset(group, "score", 0.0)
    create_str_dataset(group, "clustering_type", "unknown")
    group$close()
  }

  clusters_group$close()
}

#' Writes the projections to the H5 file
#'
#' @param f An open H5File
#' @param projections list of matrices, all with dimensions (barcodeCount x 2)
#'
#' @noRd
write_projections <- function(f, projections) {
  projections_group <- f$create_group("projections")

  for (i in seq_along(projections)) {
    name <- names(projections[i])
    projection <- projections[[i]]

    is_umap <- grepl("umap", name, ignore.case = TRUE)
    is_tsne <- grepl("tsne", name, ignore.case = TRUE)
    is_tsne_dash <- grepl("t-sne", name, ignore.case = TRUE)
    if (is_umap) {
      method <- "UMAP"
    } else if (is_tsne || is_tsne_dash) {
      method <- "t-SNE"
    } else {
      method <- name
    }

    group <- projections_group$create_group(name)
    create_str_dataset(group, "name", name)
    create_str_dataset(group, "method", method)
    create_dataset(group, "data", projection)
    group$close()
  }

  projections_group$close()
}

#' Create the metadata list
#'
#' @param seurat_obj_version optional string that holds the Seurat Object version.
#'
#' @importFrom utils sessionInfo packageVersion
#'
#' @noRd
create_metadata <- function(seurat_obj_version = NULL) {
  sinfo <- utils::sessionInfo()
  rversion <- sinfo$R.version

  # Create version string where:
  # major is a single digit, ie "4"
  # minor is multiple digits with a period, ie "2.3"
  # status is empty for release builds, but can be alpha, devel, etc
  language_version <- sprintf("%s.%s", rversion$major, rversion$minor)
  if (rversion$status != "") {
    language_version <- paste0(language_version, "-", rversion$status)
  }

  meta <- list()
  meta["tool"]             <- "loupeR"
  meta["tool_version"]     <- as.character(utils::packageVersion("loupeR"))
  meta["os"]               <- ifelse(is.null(sinfo$running), "Unknown", sinfo$running)
  meta["system"]           <- sinfo$platform
  meta["language"]         <- rversion$language
  meta["language_version"] <- language_version

  extra = list()
  extra["loupeR_seurat_version"]        <- as.character(utils::packageVersion("Seurat"))
  extra["loupeR_seurat_object_version"] <- ifelse(is.null(seurat_obj_version), "n/a", seurat_obj_version)
  extra["loupeR_hdf5r_version"]         <- as.character(utils::packageVersion("hdf5r"))
  extra["loupeR_hdf5_version"]          <- hdf5r::h5version(FALSE)
  meta[["extra"]]                       <- extra

  meta
}

#' Writes the metadata
#'
#' @param f An open H5File
#' @param seurat_obj_version optional string that holds the Seurat Object version.  It is useful for debugging compatibility issues.
#'
#' @noRd
write_metadata <- function(f, seurat_obj_version) {
  metadata <- create_metadata(seurat_obj_version)

  create_datasets <- function(parent_group, data, groupname) {
    group <- parent_group$create_group(groupname)

    for (name in names(data)) {
      val <- data[[name]]

      if (is.list(val)) {
        create_datasets(group, val, name)
      } else {
        create_str_dataset(group, name, val)
      }
    }

    group$close()
  }

  create_datasets(f, metadata, "metadata")
}

#' Create a dataset, but also closes the handle to reclaim memory
#'
#' @param obj A hdf5r File or Group
#' @param key A string name
#' @param value A vector of data
#' @param ... Additional params that will be passed to hdf5r$create_dataset
#'
#' @noRd
create_dataset <- function(obj, key, value, ...) {
  d <- obj$create_dataset(key, value, ...)
  d$close()
}

#' Create a fixed length string dataset and closes the handle to reclaim memory
#'
#' @param obj A hdf5r File or Group
#' @param key A string name
#' @param strs A vector of character data.  All Ascii characters
#' @param ... Additional params that will be passed to create_dataset
#'
#' @importFrom hdf5r H5T_STRING
#'
#' @noRd
create_str_dataset <- function(obj, key, strs, ...) {
  if (length(strs) == 0) {
    max_len <- 1
  } else {
    max_len <- max(as.numeric(lapply(strs, nchar)))
  }

  dtype <- hdf5r::H5T_STRING$new(size=max_len)

  create_dataset(obj, key, strs, dtype=dtype, ...)
}

