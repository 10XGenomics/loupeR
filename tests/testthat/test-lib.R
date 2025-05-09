create_default_seurat_obj <- function() {
  barcode_count <- 3

  count_mat <- create_count_mat(100, barcode_count, valid_barcodes = TRUE)
  proj <- create_dim_reduction(count_mat, "proj1")
  cluster <- factor(seq(barcode_count))

  obj <- Seurat::CreateSeuratObject(count_mat, assay = "rna")
  obj[["proj1"]] <- proj
  obj[["cluster1"]] <- cluster

  obj
}

setup({
  # install the eula automatically and non-interactively
  do.call(Sys.setenv, setNames(list("true"), AUTO_ACCEPT_ENV_VAR))
  eula()
})

test_that("can run create_loupe_from_seurat", {
  obj <- create_default_seurat_obj()
  x <- create_loupe_from_seurat(obj, executable_path = get_executable_path())
  expect(x, "create_loupe_from_seurat returns TRUE")
})

test_that("can run create_loupe with spaces in output_name", {
  obj <- create_default_seurat_obj()

  x <- create_loupe_from_seurat(
    obj,
    executable_path = get_executable_path(),
    output_name = "name with spaces"
  )

  expect(x, "create_loupe_from_seurat returns TRUE")
})

test_that("can run create_loupe", {
  barcode_count <- 5
  count_mat <- create_count_mat(100, barcode_count, valid_barcodes = TRUE)
  proj <- create_dense_mat(barcode_count, 2)
  projections <- list("p1" = proj)

  cluster <- factor(
    c("a", "c", "b", "a", "b"),
    levels = c("a", "b", "c"),
    ordered = TRUE
  )
  clusters <- list("f1" = cluster)

  x <- create_loupe(
    count_mat,
    clusters = clusters,
    projections = projections,
    executable_path = get_executable_path()
  )

  expect(x, "create_loupe returns TRUE")
})

test_that("can run create_loupe with BPCells", {
  barcode_count <- 5
  count_mat <- create_count_mat_BPCells(100, barcode_count, valid_barcodes = TRUE)
  proj <- create_dense_mat(barcode_count, 2)
  projections <- list("p1" = proj)

  cluster <- factor(
    c("a", "c", "b", "a", "b"),
    levels = c("a", "b", "c"),
    ordered = TRUE
  )
  clusters <- list("f1" = cluster)

  x <- create_loupe(
    count_mat,
    clusters = clusters,
    projections = projections,
    executable_path = get_executable_path()
  )

  expect(x, "create_loupe returns TRUE")
})

test_that("can run create_loupe with integer projection matrix", {
  barcode_count <- 5
  count_mat <- create_count_mat(100, barcode_count, valid_barcodes = TRUE)

  proj <- matrix(
    as.integer(create_dense_mat(barcode_count, 2) * 10),
    nrow = barcode_count,
    ncol = 2
  )

  cluster <- factor(
    c("a", "c", "b", "a", "b"),
    levels = c("a", "b", "c"),
    ordered = TRUE
  )
  clusters <- list("f1" = cluster)

  projections <- list("p1" = proj)

  x <- create_loupe(
    count_mat,
    clusters = clusters,
    projections = projections,
    executable_path = get_executable_path()
  )

  expect(x, "create_loupe returns TRUE")
})
