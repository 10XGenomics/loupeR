test_that("can run create_loupe_from_seurat", {
  barcode_count <- 3

  count_mat <- create_count_mat(100, barcode_count, valid_barcodes = TRUE)
  proj <- create_dim_reduction(count_mat, "proj1")
  cluster <- factor(seq(barcode_count))

  obj <- Seurat::CreateSeuratObject(count_mat, assay="rna")
  obj[["proj1"]] <- proj
  obj[["cluster1"]] <- cluster

  x <- create_loupe_from_seurat(obj, executable_path = get_executable_path())
  expect(x, "create_loupe_from_seurat returns TRUE")
})

test_that("can run create_loupe", {
  barcode_count <- 5
  count_mat <- create_count_mat(100, barcode_count, valid_barcodes = TRUE)
  proj <- create_dense_mat(barcode_count, 2)
  clusters <- list("f1" = factor(c("a", "c", "b", "a", "b"), levels=c("a", "b", "c"), ordered=TRUE))
  projections <- list("p1" = proj)

  x <- create_loupe(count_mat, clusters = clusters, projections = projections, executable_path = get_executable_path())
  expect(x, "create_loupe returns TRUE")
})
