test_that("are_all_close() correctly returns TRUE when comparing identical values", {
  expect_true(are_all_close(rep(1, 5), rep(1, 5), abs_tol = 1e-06, rel_tol = 1e-06))
})

test_that("are_all_close() correctly returns TRUE when comparing different values within tolerance", {
  expect_true(are_all_close(rep(1, 5) + 1e-10, rep(1, 5), abs_tol = 1e-06, rel_tol = 1e-06))
})

test_that("are_all_close() correctly returns FALSE because the relative error is above rel_tol", {
  expect_false(are_all_close(c(1, 1.5), c(1, 1), abs_tol = 1, rel_tol = 1e-06))
})

test_that("are_all_close() correctly returns FALSE because the absolute error is above abs_tol", {
  expect_false(are_all_close(c(1, 1.5), c(1, 1), abs_tol = 1e-06, rel_tol = 1))
})
