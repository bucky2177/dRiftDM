test_that("snapshot the plotting functions", {
  a_model <- dmc_dm(dx = .01, dt = .001)
  data <- simulate_data(a_model, 5000, seed = 1)
  a_model <- set_obs_data(a_model, data)
  plot1 <- function() {
    plot_traces(a_model,
      add_x = T, sigma = 0.2, k = 1,
      x_lab = "foo", y_lab = "bar",
      x_lim = c(0, 0.2), y_lim = c(-0.8, 0.8),
      line_cols_ev = c("red", "green"),
      line_cols_b = "purple", seed = 1
    )
  }
  vdiffr::expect_doppelganger(title = "trace plot", fig = plot1())

  plot2 <- function() {
    plot_cafs(a_model,
      source = "both", n_bins = 4,
      x_lab = "foo", y_lab = "bar",
      x_lim = c(-1, 5), y_lim = c(0.4, 1),
      line_cols = c("red", "green")
    )
  }
  vdiffr::expect_doppelganger(title = "caf plot", fig = plot2())

  plot3 <- function() {
    plot_quantiles(a_model,
      source = "both",
      probs = c(0.2, 0.5, 0.9),
      x_lab = "foo", y_lab = "bar",
      x_lim = c(0, 1), y_lim = c(-0.1, 1.1),
      line_cols = c("black", "blue")
    )
  }
  vdiffr::expect_doppelganger(title = "quantile plot", fig = plot3())

  plot4 <- function() {
    plot_stats(a_model, type = "delta_funs",
               source = "both",
               probs_deltas = c(0.2, 0.5, 0.7, 0.9),
               x_lab_deltas = "foo", y_lab_deltas = "bar",
               x_lim_deltas = c(0, 1), y_lim_deltas = c(-0.1, 0.3),
               line_cols = c("black"),
               minuends_deltas = "incomp", subtrahends_deltas = "comp"
    )
  }
  vdiffr::expect_doppelganger(title = "delta plot", fig = plot4())
})
