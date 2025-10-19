#' @rdname estimate_dm
#' @export
print.mcmc_dm <- function(x, ..., round_digits = drift_dm_default_rounding()) {
  chains_obj <- x

  sampler <- attr(chains_obj, "sampler")
  hierarchical <- attr(chains_obj, "hierarchical")
  subset <- get_subset_chains(chains_obj)
  n_prms <- dim(subset)[1]
  n_chains <- dim(subset)[2]
  n_iterations <- dim(subset)[3]
  cat("Sampler:", sampler, "\n")
  cat("Hierarchical:", hierarchical, "\n")
  if (hierarchical) {
    cat("No. Group-Level Parameters:", n_prms, "\n")
  } else {
    cat("No. Parameters:", n_prms, "\n")
  }
  cat("No. Chains:", n_chains, "\n")
  cat("Iterations Per Chain:", n_iterations, "\n")

  invisible(x)
}


#' Summary for `mcmc_dm` Objects
#'
#'
#' Summary and corresponding print methods for objects of the class `mcmc_dm`,
#' resulting from a call to [dRiftDM::estimate_bayesian()]. `mcmc_dm`
#' objects contain MCMC samples for Bayesian parameter estimation of
#' [dRiftDM::drift_dm()] objects. The summary includes basic parameter
#' statistics, quantiles, Gelman-Rubin diagnostics, and effective sample sizes.
#'
#' @param object an object of class `mcmc_dm`, as returned by
#' [dRiftDM::estimate_bayesian()]
#' @param x an object of class `summary.mcmc_dm`, as returned by
#'   `summary.mcmc_dm()`.
#' @param id optional single numeric or character, specifying one or more
#' participant IDs to subset `object` in the hierarchical case. Note that `id`
#' will be converted to character, because dimension names of the chains stored
#' in `object` are character. If `NULL`, then the function is applied to
#' group-level parameters.
#' @param round_digits an integer, defining the number of digits for rounding
#' the output.
#' @param show_statistics a logical, if `TRUE`, print basic parameter
#'   statistics (means, SDs, standard errors).
#' @param show_quantiles a logical, if `TRUE`, print quantile summary.
#' @param show_gr a logical; if `TRUE`, print Gelman-Rubin convergence
#'   diagnostics for each parameter.
#' @param show_eff_n a logical, if `TRUE`, print effective sample sizes for
#'   each parameter.
#' @param ... additional arguments passed forward to
#'  [coda::summary.mcmc.list()].
#'
#' @details
#'  The summary and diagnostic statistics of the MCMC chains are obtained
#'  using the R package `coda`.
#'
#'
#' @return `summary.mcmc_dm()` returns an object of class `summary.mcmc_dm`,
#'   which is a list with the following entries:
#'   - `general`: General information about the MCMC run.
#'   - `statistics`: Basic parameter summary statistics.
#'   - `quantiles`: Quantiles for each parameter.
#'   - `gr`: Gelman-Rubin diagnostics.
#'   - `eff_n`: Effective sample sizes.
#'
#' `print.summary.mcmc_dm()` prints selected summary components and returns the
#' input object invisibly.
#'
#' @examples
#' mcmc_obj <- get_example_fits("mcmc_dm")
#' print(mcmc_obj)
#' summary(mcmc_obj)
#'
#' @seealso [coda::gelman.diag()], [coda::effectiveSize()],
#'   [coda::summary.mcmc.list()]
#'
#' @export
summary.mcmc_dm <- function(object, ..., id = NULL) {
  chains_obj <- object
  ans <- list()

  # get the relevant chains array to summarize
  chains <- get_subset_chains(chains_obj = chains_obj, id = id)

  # get general Information
  general <- list()
  general$id <- id
  general$sampler <- attr(chains_obj, "sampler")
  general$hierarchical <- attr(chains_obj, "hierarchical")
  general$n_param <- dim(chains)[1]
  general$n_chains <- dim(chains)[2]
  general$n_iter <- dim(chains)[3]
  ans$general <- general

  # turn to an mcmc list to utilize the coda package
  mcmc_list <- mcmc_dm_to_coda_mcmc(chains)
  summary_coda <- summary(mcmc_list, ...)

  # Parameter Summary with respect to means/sds/ses and quantiles
  ans$statistics <- summary_coda$statistics
  ans$quantiles <- summary_coda$quantiles

  # Gelman-Rubin Statitic and Effective Sample Size
  gr_coda <- coda::gelman.diag(mcmc_list, autoburnin = FALSE)
  ans$gr <- gr_coda$psrf
  ans$eff_n <- coda::effectiveSize(mcmc_list)

  # pass back
  class(ans) <- "summary.mcmc_dm"
  return(ans)
}


#' @rdname summary.mcmc_dm
#' @export
print.summary.mcmc_dm <- function(
  x,
  ...,
  round_digits = drift_dm_default_rounding(),
  show_statistics = TRUE,
  show_quantiles = FALSE,
  show_gr = TRUE,
  show_eff_n = TRUE
) {
  summary_obj <- x

  # Print Out General Information
  general <- summary_obj$general
  id <- summary_obj$general$id

  if (!is.null(id)) {
    cat("Showing Results for ID:", id, "\n")
  }
  cat("Sampler:", general$sampler, "\n")
  cat("Hierarchical:", general$hierarchical, "\n")
  if (general$hierarchical & is.null(id)) {
    cat("No. Group-Level Parameters:", general$n_param, "\n")
  } else {
    cat("No. Parameters:", general$n_param, "\n")
  }
  cat("No. Chains:", general$n_chains, "\n")
  cat("Iterations Per Chain:", general$n_iter, "\n")

  if (show_statistics | show_quantiles | show_gr | show_eff_n) {
    cat("\n-------")
  }

  # Print Out Parameter Summaries
  statistics <- summary_obj$statistics
  if (show_statistics) {
    cat("\nParameter Summary: Basic Statistics\n")
    print(round(statistics, digits = round_digits))
  }

  # Parameter Summary with respect to quantiles
  quantiles <- summary_obj$quantiles
  if (show_quantiles) {
    cat("\nParameter Summary: Quantiles\n")
    print(round(quantiles, digits = round_digits))
  }

  # Gelman-Rubin Statistic
  gr <- summary_obj$gr
  if (show_gr) {
    cat("\nGelman-Rubin Statistics\n")
    print(round(gr[, 1], digits = round_digits))
  }

  # Effective Sample Size
  eff_n <- summary_obj$eff_n
  if (show_eff_n) {
    cat("\nEffective Sample Size\n")
    print(round(eff_n, digits = round_digits))
  }

  invisible(x)
}
