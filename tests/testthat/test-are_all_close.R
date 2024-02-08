test_that("are_all_close() works", {
  # 1) correctly returns TRUE
  # test for identical values
  expect_true(are_all_close(rep(1, 5), rep(1, 5), abs_tol = 1e-6, rel_tol = 1e-6))
  # test for similar values
  expect_true(are_all_close(rep(1, 5) + 1e-10, rep(1, 5), abs_tol = 1e-6, rel_tol = 1e-6))

  # 2) correctly returns FALSE because the relative error is above rel_tol
  expect_false(are_all_close(c(1, 1.5), c(1, 1), abs_tol = 1, rel_tol = 1e-6))

  # 3) correctly returns FALSE because the absolute error is above abs_tol
  expect_false(are_all_close(c(1, 1.5), c(1, 1), abs_tol = 1e-6, rel_tol = 1))
})
