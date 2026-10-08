# vx_match: positional fast path for dense pools, match() otherwise

test_that("vx_match agrees with match() on dense and sparse .vx", {
  dense <- 1:10
  ids <- c(3L, 10L, 1L, 11L, 0L, NA)
  expect_identical(wkpool:::vx_match(ids, dense), match(ids, dense))
  expect_identical(wkpool:::vx_match(c(2, 5), dense), match(c(2, 5), dense))
  sparse <- c(2L, 5L, 9L)
  expect_identical(wkpool:::vx_match(c(9L, 2L, 4L), sparse), match(c(9L, 2L, 4L), sparse))
  unsorted <- c(1L, 3L, 2L)
  expect_identical(wkpool:::vx_match(c(2L, 3L), unsorted), match(c(2L, 3L), unsorted))
  expect_identical(wkpool:::vx_match(integer(), dense), integer())
})

test_that("new_wkpool still rejects segment ids missing from the pool", {
  v <- data.frame(.vx = 1:3, x = 0:2, y = 0:2)
  expect_error(new_wkpool(v, 1L, 4L))
  expect_error(new_wkpool(data.frame(.vx = c(2L, 5L), x = 0:1, y = 0:1), 2L, 3L))
  expect_s3_class(new_wkpool(v, 1:2, 2:3), "wkpool")
})
