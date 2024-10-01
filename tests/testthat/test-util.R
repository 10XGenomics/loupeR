test_that("select_assay selects active assay", {
  rna1 <- create_count_mat(5, 5)
  rna2 <- create_count_mat(5, 5)
  rna3 <- create_count_mat(5, 5)

  obj <- Seurat::CreateSeuratObject(rna1, assay="rna1")
  obj[["rna2"]] = Seurat::CreateAssayObject(rna2, key="rna2_")
  obj[["rna3"]] = Seurat::CreateAssayObject(rna3, key="rna3_")

  expect_equal(Seurat::DefaultAssay(object = obj), "rna1")
  Seurat::DefaultAssay(object = obj) <- "rna2"
  expect_equal(Seurat::DefaultAssay(object = obj), "rna2")

  assay <- select_assay(obj)
  expect_equal(names(assay), "rna2")
})

test_that("counts_matrix_from_assay works on different assay version", {
  rna1 <- create_count_mat(5, 5)
  rna2 <- create_count_mat(5, 5)
  rna3 <- create_count_mat(5, 5)

  obj <- Seurat::CreateSeuratObject(rna1, assay="rna1")
  obj[["rna2"]] = Seurat::CreateAssayObject(rna2, key="rna2_")
  obj[["rna3"]] = SeuratObject::CreateAssay5Object(rna3, key="rna3_")

  expect_equal(counts_matrix_from_assay(obj[["rna1"]]), rna1)
  expect_equal(counts_matrix_from_assay(obj[["rna2"]]), rna2)
  expect_equal(counts_matrix_from_assay(obj[["rna3"]]), rna3)
})

test_that("select_clusters selects Idents", {
  rna <- create_count_mat(4, 4)
  obj <- Seurat::CreateSeuratObject(rna, assay="rna")

  # adds a new ident, old is saved at `orig.ident`
  Seurat::Idents(obj, cells = 1:4) <- c('a', 'b', 'c', 'd')

  clusters <- select_clusters(obj)
  expect_length(clusters, 2)
  expect_equal(levels(clusters[[1]]), c('a', 'b', 'c', 'd'))
})

test_that("select_clusters selects meta.data factors", {
  rna <- create_count_mat(4, 4)
  obj <- Seurat::CreateSeuratObject(rna, assay="rna")

  cell_types <- factor(c("c1", "c3", "c2", "c2"), levels=c("c1", "c2", "c3"))
  obj@meta.data['cell_types'] = cell_types

  clusters <- select_clusters(obj)
  expect_length(clusters, 3)
  expect_equal(clusters[[3]], cell_types)
})

test_that("select_clusters selects meta.data factors with deduplication", {
  rna <- create_count_mat(4, 4)
  obj <- Seurat::CreateSeuratObject(rna, assay="rna")

  cell_types <- factor(c("c1", "c3", "c2", "c2"), levels=c("c1", "c2", "c3"))
  obj@meta.data['cell_types'] = cell_types

  clusters <- select_clusters(obj, dedup=TRUE)
  expect_length(clusters, 2)
  expect_equal(clusters[[2]], cell_types)
})

test_that("select_projections select reductions", {
  rna <- create_count_mat(1000, 100)
  obj <- Seurat::CreateSeuratObject(rna, assay="rna")
  obj <- suppressWarnings(Seurat::FindVariableFeatures(obj, verbose=FALSE))
  obj <- Seurat::NormalizeData(obj, verbose=FALSE)
  obj <- Seurat::ScaleData(obj, verbose=FALSE)
  obj <- suppressWarnings(Seurat::RunPCA(obj, verbose=FALSE))
  obj <- Seurat::RunTSNE(obj, verbose=FALSE)

  projs <- select_projections(obj)
  expect_length(projs, 1)
  expect_equal(names(projs), c("tsne"))
})

test_that("deduplicate_clusters removes duplicates", {
  cell_types <- factor(c("c1", "c3", "c2", "c2"), levels=c("c1", "c2", "c3"))
  clusters <- deduplicate_clusters(list(cell_types=cell_types, clusters=cell_types))

  expect_length(clusters, 1)
  expect_equal(clusters[[1]], cell_types)
})

test_that("deduplicate_clusters prefers named factors", {
  cell_types <-  factor(c("c1", "c3", "c2", "c2"), levels=c("c1", "c2", "c3"))
  cell_types_numeric_levels <-  factor(c("1", "3", "2", "2"), levels=c("1", "2", "3"))
  clusters <- deduplicate_clusters(list(cell_types=cell_types, cell_types_numeric_levels=cell_types_numeric_levels))

  expect_length(clusters, 1)
  expect_equal(clusters[[1]], cell_types)
})
