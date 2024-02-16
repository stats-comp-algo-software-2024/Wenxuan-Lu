test_that("linalg and optim least-sq coincide", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1918)
  design <- data$design
  outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(design, outcome, model = "linear", option = list(mle_solver = "BFGS"))
  expect_true(are_all_close(coef(via_linalg_out), coef(via_bfgs_out)))
})

test_that("calculation of log likelihood gradient is correct", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", coef_true = c(1, 2, 3, 4), seed = 1)
  design <- data$design
  outcome <- data$outcome
  coef_true <- data$coef_true
  analytical_ll_gradient <- log_likelihood_gradient(regression_coef = coef_true * 0.9, design, outcome, noise_var = 1)
  approx_ll_gradient <- approx_grad(function(regression_coef) log_likelihood_func(regression_coef, design, outcome, noise_var = 1),
    coef_true * 0.9)
  expect_true(are_all_close(analytical_ll_gradient, approx_ll_gradient))
})
