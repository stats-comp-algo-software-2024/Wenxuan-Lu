test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = 'linear')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("calculation of log likelihood gradiend is correct", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', coef_true = c(1, 2, 3, 4), seed = 1)
  design <- data$design
  outcome <- data$outcome
  coef_true <- data$coef_true
  analytical_ll_gradient = log_likelihood_gradient(beta = coef_true * 0.9, design, outcome, noise_var = 1)
  approx_ll_gradient = approx_grad(function(beta) log_likelihood_func(beta, design, outcome, noise_var = 1), coef_true * 0.9)
  expect_true(are_all_close(analytical_ll_gradient, approx_ll_gradient, abs_tol = 1e-4, rel_tol = 1e-4))
})
