test_that("snapshot stats_list plots", {
  a_model <- dmc_dummy
  data <- simulate_data(a_model, n = 5000, seed = 1)
  obs_data(a_model) <- data

  all_stats <- calc_stats(a_model, c("cafs", "quantiles", "delta_funs"),
    minuends = "incomp", subtrahends = "comp"
  )

  plot_all <- function() {
    plot(all_stats, mfrow = c(1, 3), obs.pch = 21, col = c("green", "red"),
         legend.pos = "topleft", box.lwd = 2)
  }
  vdiffr::expect_doppelganger(
    title = "cafs_quantiles_delta",
    fig = plot_all
  )

  all_stats$foo <- "bar"
  expect_message({
    vdiffr::expect_doppelganger(
      title = "cafs_quantiles_delta",
      fig = plot_all
    )
  })
})


test_that("snapshot the traces plot", {
  a_model <- dmc_dummy
  traces_obj <- simulate_traces(a_model, k = 1, sigma = 0)

  plot_all_traces <- function() {
    suppress_lifecycle_deprecated(
     plot(traces_obj,
        col = c("green", "red"),
        col_b = "blue", xlim = c(-0.1, 0.4), ylim = c(-0.7, 0.7),
        xlab = "foo", ylab = "bar", lty = 2,
        legend = c("hoo", "ha"),
        legend_pos = "bottomright"
      )
    )
  }
  vdiffr::expect_doppelganger(
    title = "all_traces",
    fig = suppress_lifecycle_deprecated(plot_all_traces)
  )


  plot_all_traces <- function() {
    plot(traces_obj,
         col = c("green", "red"),
         col_b = "blue", xlim = c(-0.1, 0.4), ylim = c(-0.7, 0.7),
         xlab = "foo", ylab = "bar", b.lty = 2, pred.lty = 2,
         legend.pos = "topleft"
    )
  }
  vdiffr::expect_doppelganger(
    title = "all_traces_def_legend",
    fig = plot_all_traces
  )

  plot_one_trace <- function() {
    plot(traces_obj[[1]],
      col = c("green"),
      col_b = "blue", xlim = c(-0.1, 0.4), ylim = c(-0.7, 0.7),
      xlab = "foo", ylab = "bar", b.lty = 2, pred.lty = 2
    )
  }

  vdiffr::expect_doppelganger(
    title = "one_traces",
    fig = plot_one_trace
  )
})



test_that("snapshot for coefs.dm plot", {
  all_fits <- get_example_fits("fits_ids")

  all_prms_1 <- coef(all_fits)
  all_prms_2 <- coef(all_fits, select_unique = F)


  plot_hists1 <- function() {
    suppress_lifecycle_deprecated(
      hist(all_prms_1,
        main = c("foo", "bar", "ho"),
        colors = "blue", xlab = "test", separate_plots = TRUE
      )
    )
  }
  vdiffr::expect_doppelganger(
    title = "unique_prms_hist",
    fig = suppress_lifecycle_deprecated(plot_hists1)
  )

  plot_hists2 <- function() {
    hist(all_prms_2, alpha = 0.2, col = c("green", "red"))
  }
  vdiffr::expect_doppelganger(
    title = "cond_prms_hist",
    fig = plot_hists2
  )
})



test_that("snapshot for plot.drift_dm", {
  a_model <- ssp_dummy

  plot_model1 <- function() {
    suppress_lifecycle_deprecated(
      plot(a_model,
        conds = "comp", col = "green", xlim = c(0, 0.5),
        legend = "foo", legend_pos = "bottomright"
      )
    )
  }
  vdiffr::expect_doppelganger(
    title = "plot_model_one_cond",
    fig = suppress_lifecycle_deprecated(plot_model1)
  )

  a_model <- dmc_dummy
  comp_funs(a_model)[["x_fun"]] <- x_dirac_0
  solver(a_model) <- "im_zero"

  plot_model2 <- function() {
    plot(a_model, xlim = c(0, 0.5))
  }
  vdiffr::expect_doppelganger(
    title = "plot_model",
    fig = plot_model2
  )
})
