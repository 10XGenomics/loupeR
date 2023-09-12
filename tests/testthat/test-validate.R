test_that("validate count matrix", {
  # wrong type 
  resp <- validate_count_mat("not_a_valid_matrix")
  expect_false(resp$success)
  expect_match(resp$msg, "dgCMatrix")

  # missing feature names
  mat <- create_count_mat(10, 10)
  rownames(mat) <- NULL
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "feature dimnames")

  # missing barcode names
  mat <- create_count_mat(10, 10)
  colnames(mat) <- NULL
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "barcodes dimnames")

  # NaN values
  mat <- create_count_mat(10, 10)
  mat@x[[1]] <- NaN # set first value to NaN
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "not be NaN")

  # infinite
  mat <- create_count_mat(10, 10)
  mat@x[[1]] <- Inf
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "not be infinite")

  # bad barcodes
  mat <- create_count_mat(10, 10)
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "There is an issue with the formatting of your barcodes")

  # non unique barcodes
  mat <- create_count_mat(3, 3)
  colnames(mat) <- c("ACTGAA-1", "ACTGAA-1", "ACTGAA-1")
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "should be unique")

  # good
  mat <- create_count_mat(3, 3)
  colnames(mat) <- c("ACTGAA-1", "ACTAAA-1", "CTAGAA-1")
  resp <- validate_count_mat(mat)
  expect_true(resp$success)

  # bad, but fixable  barcodes
  barcodes <- c("prefix-ACTGAA-1", "prefix-ACTAAA-1", "prefix-CTAGAA-1")
  mat <- create_count_mat(3, 3)
  colnames(mat) <- barcodes
  resp <- validate_count_mat(mat)
  expect_true(resp$success)
})

test_that("validate clusters", {
  # wrong type
  resp <- validate_clusters("not_a_valid_cluster_list", 3)
  expect_false(resp$success)
  expect_match(resp$msg, "must be in a list")

  # empty list
  resp <- validate_clusters(list(), 3)
  expect_false(resp$success)
  expect_match(resp$msg, "at least one")

  # clusters should be factors
  resp <- validate_clusters(list("not a factor"), 3)
  expect_false(resp$success)
  expect_match(resp$msg, "must all be factors")

  # clusters supply names
  resp <- validate_clusters(list(factor(c("one", "two", "three"))), 3)
  expect_false(resp$success)
  expect_match(resp$msg, "must supply names")

  # empty name
  factors <- list()
  f <- factor(c("one", "two", "three"))
  factors[[""]] = f
  resp <- validate_clusters(factors, 3)
  expect_false(resp$success)
  expect_match(resp$msg, "cannot be the empty string")

  # wrong barcode count
  factors <- list("f1" = factor(c("one", "two", "three")))
  resp <- validate_clusters(factors, 42)
  expect_false(resp$success)
  expect_match(resp$msg, "same length as the number of barcodes")

  # good
  factors <- list("f1" = factor(c("one", "two", "three")))
  resp <- validate_clusters(factors, 3)
  expect_true(resp$success)
})

test_that("validate projections", {
  barcode_count <- 3

  # wrong type
  resp <- validate_projections("not_a_valid_projection_list", 3)
  expect_false(resp$success)
  expect_match(resp$msg, "must be in a list")

  # empty list
  resp <- validate_projections(list(), 3)
  expect_false(resp$success)
  expect_match(resp$msg, "at least one")

  # projections should be matrix
  resp <- validate_projections(list("not a matrix"), 3)
  expect_false(resp$success)
  expect_match(resp$msg, "must all be matrices")

  # projections supply names
  proj <- create_dense_mat(barcode_count, 2)
  resp <- validate_projections(list(proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "must supply names")

  # empty name
  projs <- list()
  proj <- create_dense_mat(barcode_count, 2)
  projs[[""]] = proj
  resp <- validate_projections(projs, barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "cannot be the empty string")

  # mixed dimensions
  proj <- create_dense_mat(2, barcode_count)
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "must all have dimensions")

  # no NaN
  proj <- create_dense_mat(barcode_count, 2)
  proj[1,1] <- NaN
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "must not contain NaN")

  # no infinite
  proj <- create_dense_mat(barcode_count, 2)
  proj[1,1] <- Inf 
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "infinite values")

  # good 
  proj <- create_dense_mat(barcode_count, 2)
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_true(resp$success)
})
