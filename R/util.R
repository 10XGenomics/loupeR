# Environment variable that allows overwriting the user data directory
# If unset, defaults to: tools::R_user_dir("loupeR", "data")
LOUPER_USER_DATA_DIR_ENV_VAR <- "LOUPER_USER_DATA_DIR" # nolint

#' Log a message
#'
#' @param ... a variable number of character message parts
#'
#' @noRd
log_msg <- function(...) {
  l <- list(...)
  if (length(l) == 0) {
    return()
  }

  now <- format(Sys.time(), format = "%Y/%m/%d %H:%M:%S", usetz = FALSE)

  msg_vector <- now
  for (part in l) {
    msg_vector <- c(msg_vector, part)
  }

  msg <- do.call(paste, as.list(msg_vector))

  message(msg)
}

#' Select the "best" assay for conversion
#'
#' @description
#' Prioritizes the active assay, then RNA, and then the rest
#' Usable assays must have a non empty count matrix
#'
#' @param obj A Seurat Object
#'
#' @return A list with the named Seurat Assay or NULL if not found
#'
#' @importFrom Seurat Assays
#' @importFrom Seurat GetAssay
#'
#' @export
select_assay <- function(obj) {
  # Search the assays for the best match for the count matrix
  # Prioritize active assay, RNA, and then the rest
  assay_priority <- c()
  for (name in Seurat::Assays(obj)) {
    if (identical(name, obj@active.assay)) {
      priority <- 1
    } else if (grepl("rna", name, ignore.case = TRUE)) {
      priority <- 2
    } else {
      priority <- 3
    }

    assay_priority[name] <- priority
  }
  assay_priority <- sort(assay_priority)

  assay <- NULL
  for (i in seq_along(assay_priority)) {
    name <- names(assay_priority[i])
    assay <- Seurat::GetAssay(obj, assay = name)
    counts <- counts_matrix_from_assay(assay)

    if (length(counts) > 0) {
      result <- list()
      result[[name]] <- assay
      return(result)
    }
  }

  NULL
}

#' Extract the counts matrix from the Assay
#'
#' @param assay A SeuratObject::Assay or SeuratObject::Assay5
#'
#' @return A sparse counts matrix
#'
#' @export
counts_matrix_from_assay <- function(assay) {
  if (packageVersion("Seurat") >= package_version("5.0.0")) {
    assay$counts
  } else {
    if (is(assay, "Assay5")) {
      stop("Cannot get count matrix: Please upgrade to Seurat > 5 to support dataset")
    }

    assay@counts
  }
}

#' Select clusters from the assay
#'
#' @param obj A Seurat Object
#' @param dedup logical to dedupicate clusters.  Default TRUE.
#' @param metadata_cols optional list that specifies which metadata columns to retain when selecting clusters
#'
#' @return A list of factors
#'
#' @importFrom Seurat Idents
#'
#' @export
select_clusters <- function(
    obj,
    dedup = FALSE,
    metadata_cols = NULL) {
  # Use the active.ident as a cluster
  clusters <- list(active_cluster = Seurat::Idents(obj))

  # Use all factors and character vectors from meta.data
  for (name in names(obj@meta.data)) {
    if (!is.null(metadata_cols) && !(name %in% metadata_cols)) {
      next
    }

    data <- obj@meta.data[[name]]

    if (is.factor(data)) {
      clusters[[name]] <- data
    } else if (is.character(data)) {
      clusters[[name]] <- factor(data)
    }
  }

  # remove clusters that are missing factor levels or have a single empty level name
  good_clusters <- sapply(clusters, function(f) {
    if (length(levels(f)) == 0) {
      return(FALSE)
    }
    max(as.numeric(lapply(levels(f), nchar))) > 0
  })

  clusters <- clusters[good_clusters]

  if (dedup) deduplicate_clusters(clusters) else clusters
}

#' Select projections from the assay
#'
#' @param obj A Seurat Object
#'
#' @return A list of matrices, all with dimensions (barcodeCount x 2)
#'
#' @importFrom Seurat Reductions
#'
#' @export
select_projections <- function(obj) {
  projections <- list()
  for (name in Seurat::Reductions(obj)) {
    reduction <- obj[[name]]
    if (dim(reduction@cell.embeddings)[[2]] == 2) {
      projections[[name]] <- reduction@cell.embeddings
    }
  }

  projections
}

#' Read FeatureIds from 10x features.tsv.gz file
#'
#' @param tsv_path character vector path to the features.tsv.gz file
#'
#' @return A character vector of the feature ids
#'
#' @importFrom utils read.csv
#'
#' @export
read_feature_ids_from_tsv <- function(tsv_path) {
  tsv_path <- normalizePath(path.expand(tsv_path))
  df <- utils::read.csv(tsv_path, sep = "\t", header = FALSE)
  feature_ids <- df[[1]]

  feature_ids
}

#' Check Clusters are identical numerically
#'
#' @param x factor that hold information for each barcode
#' @param y factor that hold information for each barcode
#'
#' @return TRUE if identical FALSE otherwise
#'
#' @noRd
clusters_identical <- function(x, y) {
  # relevel so that factors are in order of appearance, then convert to numeric
  x <- as.numeric(factor(x, levels = as.character(unique(x))))
  y <- as.numeric(factor(y, levels = as.character(unique(y))))
  all(x == y)
}

#' Deduplicate clusters
#'
#' @param clusters list of factors that hold information for each barcode
#'
#' @return list of clusters with duplicated removed
#'
#' @noRd
deduplicate_clusters <- function(clusters) {
  if (length(clusters) <= 1) {
    return(clusters)
  }

  # grouping of the cluster names with identical data
  groups <- list()
  for (clusterIdx in seq_along(clusters)) {
    cluster_with_name <- clusters[clusterIdx]
    cluster <- clusters[[clusterIdx]]

    # find a matching group and potentially add this cluster to it.
    found <- FALSE
    for (groupIdx in seq_along(groups)) {
      group <- groups[[groupIdx]]
      group_cluster <- group[[1]]
      if (clusters_identical(cluster, group_cluster)) {
        group <- c(group, cluster_with_name)
        groups[[groupIdx]] <- group
        found <- TRUE
        break
      }
    }

    # if no matching group, create a new group with this cluster
    if (!found) {
      groups <- c(groups, list(cluster_with_name))
    }
  }

  final_clusters <- list()

  # choose one cluster from each group, prioritizing those with named factors
  for (group in groups) {
    named_cluster_idx <- Find(function(i) {
      cluster_levels_word_like(group[[i]])
    }, seq_along(group))

    if (!(is.null(named_cluster_idx))) {
      final_clusters <- c(final_clusters, group[named_cluster_idx])
    } else {
      final_clusters <- c(final_clusters, group[1])
    }
  }

  final_clusters
}

#' Checks if any of the cluster levels are word-like.
#'
#' @description
#' It is quite common for Seurat clusters to have values that are integers.
#' The are stored as characters, but can be easily parsed into numbers.
#' We want to prioritize clusters that have things like cell type names added.
#'
#' @param cluster A factor
#'
#' @return boolean if factor has names
#'
#' @noRd
cluster_levels_word_like <- function(cluster) {
  lvls <- levels(cluster)
  suppressWarnings({
    any(is.na(as.numeric(lvls)))
  })
}

#' Gets the systems OS.
#'
#' @return "windows", "mac", "unix"
#'
#' @noRd
get_system_os <- function() {
  if (.Platform$OS.type == "windows") {
    os <- "windows"
  } else {
    platform <- R.Version()$platform

    if (grepl("apple", platform, ignore.case = TRUE) || grepl("darwin", platform, ignore.case = TRUE)) {
      os <- "mac"
    } else {
      os <- "unix"
    }
  }

  os
}

print_lines <- function(strs, prefix = "") {
  for (s in strs) {
    cat(sprintf("%s%s\n", prefix, s))
  }
}

#' Gets the path to the user directory on the machine where data is stored.
#'
#' @return A filesystem path to the user directory where things like the EULA can be stored.
#'
#' @noRd
get_user_data_dir <- function() {
  overwritten_data_dir <- Sys.getenv(LOUPER_USER_DATA_DIR_ENV_VAR)
  overwritten_data_dir <- trimws(overwritten_data_dir)
  if (overwritten_data_dir != "") {
    return(overwritten_data_dir)
  }

  tools::R_user_dir("loupeR", "data")
}
