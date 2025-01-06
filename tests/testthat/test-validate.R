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
  colnames(mat) <- c("AAACATACAAACAG-1", "AAACATACAAACAG-1", "AAACATACAAACAG-1")
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "should be unique")

  # empty barcode
  mat <- create_count_mat(3, 3)
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "")
  resp <- validate_count_mat(mat)
  expect_false(resp$success)
  expect_match(resp$msg, "cannot be the empty string")

  # empty feature names
  mat <- create_count_mat(3, 3)
  rownames(mat) <- c("feat-1", "feat-2", "")
  resp <- validate_count_mat(mat)
  expect_false(resp$success)

  # good
  mat <- create_count_mat(3, 3)
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "ACACATACAAACAG-1")
  resp <- validate_count_mat(mat)
  expect_true(resp$success)

  # bad
  barcodes <- c("prefix-AAACATACAAACAG-1", "prefix-AGACATACAAACAG-1", "prefix-ACACATACAAACAG-1")
  mat <- create_count_mat(3, 3)
  colnames(mat) <- barcodes
  resp <- validate_count_mat(mat)
  expect_false(resp$success)

  # feature_ids good
  mat <- create_count_mat(3, 3)
  feature_ids <- c("one", "two", "three")
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "ACACATACAAACAG-1")
  resp <- validate_count_mat(mat, feature_ids = feature_ids)
  expect_true(resp$success)

  # feature_ids wrong size
  mat <- create_count_mat(3, 3)
  feature_ids <- c("one", "two")
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "ACACATACAAACAG-1")
  resp <- validate_count_mat(mat, feature_ids = feature_ids)
  expect_false(resp$success)

  # feature_ids not unique
  mat <- create_count_mat(3, 3)
  feature_ids <- c("one", "one", "one")
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "ACACATACAAACAG-1")
  resp <- validate_count_mat(mat, feature_ids = feature_ids)
  expect_false(resp$success)

  # feature_ids empty string
  mat <- create_count_mat(3, 3)
  feature_ids <- c("one", "two", "")
  colnames(mat) <- c("AAACATACAAACAG-1", "AGACATACAAACAG-1", "ACACATACAAACAG-1")
  resp <- validate_count_mat(mat, feature_ids = feature_ids)
  expect_false(resp$success)
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
  factors[[""]] <- f
  resp <- validate_clusters(factors, 3)
  expect_false(resp$success)
  expect_match(resp$msg, "cannot be the empty string")

  # wrong barcode count
  factors <- list("f1" = factor(c("one", "two", "three")))
  resp <- validate_clusters(factors, 42)
  expect_false(resp$success)
  expect_match(resp$msg, "same length as the number of barcodes")

  # too many groupings
  factors <- list("f1" = factor(seq(32769)))
  resp <- validate_clusters(factors, length(factors[[1]]))
  expect_false(resp$success)
  expect_match(resp$msg, "cluster cannot have more than")

  # cluster with zero levels
  factors <- list("f1" = factor(seq(32769), levels = c()))
  resp <- validate_clusters(factors, length(factors[[1]]))
  expect_false(resp$success)
  expect_match(resp$msg, "cluster must have at least one grouping")

  # cluster with one level, but it's empty
  factors <- list("f1" = factor(seq(32769), levels = c("")))
  resp <- validate_clusters(factors, length(factors[[1]]))
  expect_false(resp$success)
  expect_match(resp$msg, "cluster group names cannot be the empty string")

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
  projs[[""]] <- proj
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
  proj[1, 1] <- NaN
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "must not contain NaN")

  # no infinite
  proj <- create_dense_mat(barcode_count, 2)
  proj[1, 1] <- Inf
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_false(resp$success)
  expect_match(resp$msg, "infinite values")

  # good
  proj <- create_dense_mat(barcode_count, 2)
  resp <- validate_projections(list("p1" = proj), barcode_count)
  expect_true(resp$success)
})

test_that("validate_barcodes checks barcodes for proper formatting", {
  # no change
  expect_true(validate_barcodes("AAACATACAAACAG")$success)

  # no change + lane numbers
  expect_true(validate_barcodes("AAACATACAAACAG-1")$success)

  # prefix
  expect_true(validate_barcodes("prefix_AAACATACAAACAG")$success)
  expect_true(validate_barcodes("prefix:AAACATACAAACAG")$success)
  expect_false(validate_barcodes("prefix-AAACATACAAACAG")$success)

  # barcodes with lane numbers + prefix
  expect_true(validate_barcodes("prefix_AAACATACAAACAG-1")$success)
  expect_true(validate_barcodes("prefix:AAACATACAAACAG-1")$success)
  expect_false(validate_barcodes("prefix-AAACATACAAACAG-1")$success)

  # barcodes + prefix_with_underscore
  expect_true(validate_barcodes("pre_fix_AAACATACAAACAG")$success)
  expect_true(validate_barcodes("pre_fix:AAACATACAAACAG")$success)
  expect_false(validate_barcodes("pre_fix-AAACATACAAACAG")$success)

  # other various test cases for the barcode
  expect_true(validate_barcodes("_AAACCTGAGAAACG")$success)
  expect_true(validate_barcodes("AAACCTGAGAAACG_")$success)
  expect_true(validate_barcodes("_AAACCTGAGAAACG_")$success)
  expect_true(validate_barcodes(":AAACCTGAGAAACG")$success)
  expect_true(validate_barcodes("AAACCTGAGAAACG:")$success)
  expect_true(validate_barcodes(":AAACCTGAGAAACG:")$success)
  expect_true(validate_barcodes("abc-:AAACCTGAGAAACG:-def")$success)
  expect_true(validate_barcodes("AAACATACAAACAG-1")$success)
  expect_false(validate_barcodes("abcAAACATACAAACAGxyz")$success)
  expect_false(validate_barcodes("AAACATACAAACA")$success)
  expect_false(validate_barcodes("AAACATACAAACAD")$success)
  expect_false(validate_barcodes("aaacatacaaaacag")$success)
  expect_false(validate_barcodes("-aaacatacaaaacag")$success)
  expect_false(validate_barcodes("aaacatacaaaacag-")$success)

  # barcodes with lane numbers + prefix_with_underscore
  expect_true(validate_barcodes("pre_fix_AAACATACAAACAG-1")$success)
  expect_true(validate_barcodes("pre_fix:AAACATACAAACAG-1")$success)
  expect_false(validate_barcodes("pre_fix-AAACATACAAACAG-1")$success)

  # barcodes with prefix_with_underscore and suffix_with_underscore
  expect_true(validate_barcodes("pre_fix_AAACATACAAACAG_suf_fix")$success)
  expect_true(validate_barcodes("pre_fix:AAACATACAAACAG:suf_fix")$success)
  expect_false(validate_barcodes("pre_fix-AAACATACAAACAG-suf_fix")$success)

  # barcodes with lane numbers with prefix_with_underscore and suffix_with_underscore
  expect_true(validate_barcodes("pre_fix_AAACATACAAACAG-1_suf_fix")$success)
  expect_true(validate_barcodes("pre_fix:AAACATACAAACAG-1:suf_fix")$success)

  # unsupported suffixes and prefixes
  expect_false(validate_barcodes("pre_fix-AAACATACAAACAG-1-suf_fix")$success)
  expect_false(validate_barcodes("pre_fix:AAACATACAAACAG-1-suf_fix")$success)
  expect_false(validate_barcodes("pre_fix-AAACATACAAACAG:1-suf_fix")$success)

  # visium hd suffixes and prefixes
  expect_true(validate_barcodes("s_008um_00425_00829")$success)
  expect_true(validate_barcodes("s_000um_00000_00000")$success)
  expect_true(validate_barcodes("_s_008um_00425_00829")$success)
  expect_true(validate_barcodes("s_008um_00425_00829_")$success)
  expect_true(validate_barcodes("_s_008um_00425_00829_")$success)
  expect_true(validate_barcodes(":s_008um_00425_00829")$success)
  expect_true(validate_barcodes("s_008um_00425_00829:")$success)
  expect_true(validate_barcodes(":s_008um_00425_00829:")$success)
  expect_true(validate_barcodes("abc-:s_008um_00425_00829:-def")$success)

  expect_false(validate_barcodes("abcs_000um_00000_00000xyz")$success)
  expect_false(validate_barcodes("s_008um_00425_00829-")$success)
  expect_false(validate_barcodes("a008um_00425_00829")$success)
  expect_false(validate_barcodes("a_008um_00425_00829")$success)
  expect_false(validate_barcodes("s_01um_00425_00829")$success)
  expect_false(validate_barcodes("s_001am_00425_00829")$success)
  expect_false(validate_barcodes("s_008ux_00425_00829")$success)
  expect_false(validate_barcodes("s_008um00425_00829")$success)
  expect_false(validate_barcodes("s_008um_0425_00829")$success)
  expect_false(validate_barcodes("s_008um_0042500829")$success)
  expect_false(validate_barcodes("s_008um_00425_0829")$success)
  expect_false(validate_barcodes("abcs_008um_00425_0082xyz")$success)

  # visium hd with lane numbers, prefixes and suffixes
  expect_true(validate_barcodes("s_008um_00425_00829-1")$success)
  expect_true(validate_barcodes("s_000um_00000_00000-2")$success)
  expect_true(validate_barcodes("s_000um_00000_00000-10000")$success)

  expect_true(validate_barcodes("_s_008um_00425_00829-1")$success)
  expect_true(validate_barcodes("s_008um_00425_00829-2_")$success)
  expect_true(validate_barcodes("_s_008um_00425_00829-03_")$success)
  expect_true(validate_barcodes(":s_008um_00425_00829-004")$success)
  expect_true(validate_barcodes("s_008um_00425_00829-59:")$success)
  expect_true(validate_barcodes(":s_008um_00425_00829-0008:")$success)
  expect_true(validate_barcodes("abc-:s_008um_00425_00829-0:-def")$success)

  expect_false(validate_barcodes("asdfs_000um_00000_00000-10000")$success)
  expect_false(validate_barcodes("abcs_000um_00000_00000-1xyz")$success)
  expect_false(validate_barcodes("a008um_00425_00829-1")$success)
  expect_false(validate_barcodes("a_008um_00425_00829-1")$success)
  expect_false(validate_barcodes("s_01um_00425_00829-1")$success)
  expect_false(validate_barcodes("s_001am_00425_00829-1")$success)
  expect_false(validate_barcodes("s_008ux_00425_00829-1")$success)
  expect_false(validate_barcodes("s_008um00425_00829-1")$success)
  expect_false(validate_barcodes("s_008um_0425_00829-1")$success)
  expect_false(validate_barcodes("s_008um_0042500829-1")$success)
  expect_false(validate_barcodes("s_008um_00425_0829-1")$success)
  expect_false(validate_barcodes("s_008um_00425_00829-")$success)
  expect_false(validate_barcodes("s_008um_00425_00829-a")$success)
  expect_false(validate_barcodes("abcs_000um_00000_00000-xyz")$success)

  # xenium cell ids
  expect_true(validate_barcodes("ffkpbaba-1")$success)
  expect_true(validate_barcodes("a-1")$success)
  expect_true(validate_barcodes("abc-100")$success)

  expect_true(validate_barcodes("_ffkpbaba-1")$success)
  expect_true(validate_barcodes("ffkpbaba-3_")$success)
  expect_true(validate_barcodes("_ffkpbaba-3_")$success)
  expect_true(validate_barcodes(":jjkpbaba-1")$success)
  expect_true(validate_barcodes("ffkpbaba-009:")$success)
  expect_true(validate_barcodes(":ffkpbaba-123:")$success)
  expect_true(validate_barcodes("abc-:ffkpbaba-1:-def")$success)

  expect_false(validate_barcodes("ffkpbabq-10000")$success)
  expect_false(validate_barcodes("abcs_ffkpbaba-1xyz")$success)
  expect_false(validate_barcodes("affkpbaba-1")$success)
})
