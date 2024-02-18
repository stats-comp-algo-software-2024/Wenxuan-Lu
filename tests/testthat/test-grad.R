test_that("linear model's analytical log likelihood gradient is close to numerical one", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", coef_true = c(1, 2, 3, 4), seed = 1)
  design <- data$design
  outcome <- data$outcome
  coef_true <- data$coef_true
  analytical_ll_gradient <- log_likelihood_gradient_linear(regression_coef = coef_true * 0.9, design,
    outcome, noise_var = 1)
  approx_ll_gradient <- approx_grad_via_finite_diff(function(regression_coef) log_likelihood_func_linear(regression_coef,
    design, outcome, noise_var = 1), coef_true * 0.9)
  expect_true(are_all_close(analytical_ll_gradient, approx_ll_gradient, abs_tol = 1e-04, rel_tol = 1e-04))
})

test_that("logit model's analytical log likelihood gradient is close to numerical one", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", coef_true = c(1, 2, 3, 4), seed = 1)
  design <- data$design
  outcome <- data$outcome
  coef_true <- data$coef_true
  analytical_ll_gradient <- log_likelihood_gradient_logit(regression_coef = coef_true * 0.9, design,
    outcome)
  approx_ll_gradient <- approx_grad_via_finite_diff(function(regression_coef) log_likelihood_func_logit(regression_coef,
    design, outcome), coef_true * 0.9)
  expect_true(are_all_close(analytical_ll_gradient, approx_ll_gradient))
})
