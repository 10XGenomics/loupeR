test_that("can create hdf5", {
  barcode_count <- 5
  proj <- create_dense_mat(barcode_count, 2)
  count_mat <- create_count_mat(100, barcode_count)
  clusters <- list("f1" = factor(c("a", "c", "b", "a", "b"), levels=c("a", "b", "c"), ordered=TRUE))
  projections <- list("p1" = proj)
  h5path <- sprintf("%s.h5", tempfile())

  seurat_obj_version <- "1.2.3"
  create_hdf5(count_mat, clusters, projections, h5path, seurat_obj_version)

  f <- hdf5r::h5file(h5path)

  # spot check matrix
  matrix_group <- hdf5r::openGroup(f, "matrix")
  barcodes <- hdf5r::openLocation(matrix_group, "barcodes")
  data <- hdf5r::openLocation(matrix_group, "data")
  expect_equal(hdf5r::readDataSet(barcodes), paste0("col", 1:barcode_count))
  expect_equal(hdf5r::readDataSet(data), count_mat@x)

  features_group <- hdf5r::openGroup(matrix_group, "features")
  feature_names <- hdf5r::openGroup(features_group, "name")
  expect_equal(hdf5r::readDataSet(feature_names), paste0("row", 1:100))

  # spot check projections
  projs_group <- hdf5r::openGroup(f, "projections")
  proj_group <- hdf5r::openGroup(projs_group, "p1")
  proj_data <- hdf5r::openLocation(proj_group, "data")
  expect_equal(proj, hdf5r::readDataSet(proj_data))

  # spot check clusters
  clusters_group <- hdf5r::openGroup(f, "clusters")
  cluster_group <- hdf5r::openGroup(clusters_group, "f1")
  assignments <- hdf5r::openLocation(cluster_group, "assignments")
  group_names <- hdf5r::openLocation(cluster_group, "group_names")
  expect_equal(hdf5r::readDataSet(assignments), clusters[[1]]@.Data - 1)
  expect_equal(hdf5r::readDataSet(group_names), levels(clusters[[1]]))

  # spot check metadata
  metadata <- hdf5r::openGroup(f, "metadata")

  tool <- hdf5r::openLocation(metadata, "tool")
  expect_equal(hdf5r::readDataSet(tool), "loupeR")

  extra <- hdf5r::openGroup(metadata, "extra")
  loupeR_seurat_version <- hdf5r::openLocation(extra, "loupeR_seurat_version")
  val <- hdf5r::readDataSet(loupeR_seurat_version)
  expect(!is.null(hdf5r::readDataSet(loupeR_seurat_version)), "extra field is missing")
})
