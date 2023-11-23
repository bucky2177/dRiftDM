test_that("force negative pdf values", {
  a_model = ratcliff_dm(dt = .01, dx = .01, obs_data = ratcliff_data)
  a_model = set_model_prms(a_model, c(7, 0.1, 0.3))
  retur_val = suppressWarnings(get_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(get_log_like(a_model), "subst. negative pdf")

  a_model = set_model_prms(a_model, c(7, 0.14, 0.3))
  retur_val = suppressWarnings(get_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(
    expect_warning(
      expect_warning(
        get_log_like(a_model), "negative density values"
        ), "NaNs"
      ), "NaNs"
    )

  a_model = ratcliff_dm(dt = .01, dx = .01)
  retur_val = suppressWarnings(get_log_like(a_model))
  expect_identical(-Inf, retur_val)
  expect_warning(get_log_like(a_model), "no data is provided")

})




# get an x of the wrong dimension
x.foo =  function(drift_dm_obj, x_vec, one_cond) {
  dx <- drift_dm_obj$prms_solve[["dx"]]
  vec = c(1,1,1,1) / (4*dx)
  return(vec)
}

# get wrong mu
mu.foo = function(drift_dm_obj, t_vec, one_cond) {
  mu <- standard_drift()
  if (!is.numeric(t_vec) | length(t_vec) <= 1) {
    stop("t_vec is not a vector")
  }
  mu <- rep(mu, length(t_vec) - 3)
  return(mu)
}

# get wrong b
b.foo <- function(drift_dm_obj, t_vec, one_cond) {
  b <- standard_boundary()
  b <- rep(b, length(t_vec) - 3)
  return(b)
}

# get wrong dt_b
dt_b.foo <- function(drift_dm_obj, t_vec, one_cond) {
  dt_b <- rep(0, length(t_vec) - 3)
  return(dt_b)
}

test_that("call pdfs with unreasonable components", {

  print(search())

  a_model = ratcliff_dm()
  class(a_model) = c("foo", class(a_model))

  expect_error(get_pdfs(a_model, one_cond = "null", solver = "kfe"),
               "unexpected length of x_vals")
  rm(x.foo)


  expect_error(get_pdfs(a_model, one_cond = "null", solver = "kfe"),
               "unexpected length of mu_vals")
  rm(mu.foo)


  expect_error(get_pdfs(a_model, one_cond = "null", solver = "kfe"),
               "unexpected length of b_vals")
  rm(b.foo)

rror(get_pdfs(a_model, one_cond = "null", solver = "kfe"),
               "unexpected length of dt_b_vals")
  rm(dt_b.foo)
})



a = function() {
  list(fun = function() rnorm(1))
}
