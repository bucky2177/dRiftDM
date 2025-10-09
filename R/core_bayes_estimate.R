# HELPER FUNCTIONS REQUIRED FOR MCMC SAMPLERS ---------------------------


#' Perform a Migration Step Between Chains
#'
#' In a migration step, a random subset of chains is selected. Each selected
#' chain `k` receives a proposal based on the next chain `(k + 1)` in the
#' sequence (cycling back to the first chain after the last). The proposed
#' parameters are slightly perturbed by uniform noise controlled by `b`.
#' All proposals are then evaluated simultaneously using Metropolis acceptance
#' probabilities via [dRiftDM::call_log_posterior_m()], and accepted proposals
#' replace the current values.
#'
#' @param prms_across_chains a numeric matrix of dimension `p × n`, where `p`
#'   is the number of parameters and `n` is the number of chains. Each column
#'   contains the current parameter vector of a chain.
#' @param pis_across_chains a numeric vector of length `n`, containing the
#'   current log-posterior values for each chain.
#' @param log_likes_across_chains a numeric vector of length `n`, containing the
#'   current log-likelihood values for each chain.
#' @param b a small numeric value used to perturb the proposal parameters to
#'   avoid degeneracy.
#' @param ... additional arguments passed to [dRiftDM::call_log_posterior_m()].
#'
#' @return A list with the following components:
#' * `new_prms_across_chains`: The updated parameter matrix of shape `p × n`.
#' * `new_pis_across_chains`: The updated vector of log-posterior values.
#' * `new_log_likes_across_chains`: The updated vector of log-likelihood values.
#'
#' @seealso [dRiftDM::full_crossover], [dRiftDM::call_log_posterior_m()]
#' @keywords internal
migration_crossover <- function(prms_across_chains, pis_across_chains,
                                log_likes_across_chains, b = 0.001, ...) {

  n_chains <- ncol(prms_across_chains)
  n_prms <- nrow(prms_across_chains)

  # Choose a subset of chains to perform migration
  eta <- sample(n_chains, 1)
  selected_chains <- sample(n_chains, eta)

  # Get migration targets (cycled indices)
  target_chains <- selected_chains + 1
  target_chains[target_chains > n_chains] <- 1

  # Generate proposal matrix: target chain values + small noise
  proposal_mat <- prms_across_chains[, target_chains, drop = FALSE] +
    matrix(stats::runif(n_prms * eta, -b, b), nrow = n_prms, ncol = eta)

  # Extract previous prms, pis, lls for the selected chains
  prev_prms_mat <- prms_across_chains[, selected_chains, drop = FALSE]
  prev_pis <- pis_across_chains[selected_chains]
  prev_lls <- log_likes_across_chains[selected_chains]


  # Capture and filter dots to reduce the matrices on which we condition
  dots <- list(...)
  # (theta_j_mat has chains on rows and subjs on columns)
  if (!is.null(dots$theta_j_mat)) {
    dots$theta_j_mat <- dots$theta_j_mat[selected_chains, , drop = FALSE]
  }
  # (all_phis_mat has parameters on rows and chains on columns)
  if (!is.null(dots$all_phis_mat)) {
    dots$all_phis_mat <- dots$all_phis_mat[, selected_chains, drop = FALSE]
  }
  # select the relevant temperatures
  if (!is.null(dots$temperatures)) {
    dots$temperatures <- dots$temperatures[selected_chains]
  }

  # Evaluate log-posterior for proposals
  results <- do.call(call_log_posterior_m, c(
    list(
      proposal_mat = proposal_mat,
      prev_prms_mat = prev_prms_mat,
      prev_pis = prev_pis,
      prev_lls = prev_lls
    ),
    dots
  ))

  # Update parameter matrix if proposal accepted
  accepted <- results$accept
  which_acc = selected_chains[accepted]
  prms_across_chains[, which_acc] <- proposal_mat[, accepted, drop = FALSE]
  pis_across_chains[which_acc] <- results$pis[accepted]
  log_likes_across_chains[which_acc] <- results$lls[accepted]

  # wrap it up and return
  return_list = list(
    new_prms_across_chains = prms_across_chains,
    new_pis_across_chains = pis_across_chains,
    new_log_likes_across_chains = log_likes_across_chains
  )
  return(return_list)
}



#' Perform a Full Crossover Step Using Differential Evolution
#'
#' This function updates each chain's parameters by proposing new values using a
#' differential evolution strategy. For each chain `k`, two other chains
#' `m` and `n` are randomly selected, and a proposal is generated via:
#' \code{prms_k + gamma * (prms_m - prms_n) + noise},
#'       where \code{gamma = 2.38 / sqrt(2 * n_prms)}
#' and \code{noise} is uniform perturbation controlled by `b`. The proposal is
#' accepted with Metropolis probability via [dRiftDM::call_log_posterior_m()],
#' and accepted proposals replace the current values.
#'
#' @inheritParams migration_crossover
#' @param gamma a single numeric tuning parameter, that scales the difference
#' between parameters. If `NULL`, defaults to `2.38 / sqrt(2 * n_prms)`
#'
#' @inherit migration_crossover return
#'
#' @keywords internal
full_crossover <- function(prms_across_chains, pis_across_chains,
                           log_likes_across_chains, gamma = NULL,
                           b = 0.001, ...) {

  n_chains <- ncol(prms_across_chains)
  n_prms <- nrow(prms_across_chains)
  if (is.null(gamma)) gamma <- 2.38 / sqrt(2 * n_prms)

  # Sample chain pairs: matrix of size (2 x n_chains)
  all_chains = seq_len(n_chains)
  selected_mn <- vapply(all_chains, function(k) {
    sample(all_chains[-k], 2)
  }, FUN.VALUE = rep(1L, 2))


  # Get crossover proposals
  prms_m_mat <- prms_across_chains[, selected_mn[1,]]
  prms_n_mat <- prms_across_chains[, selected_mn[2,]]
  noise <- matrix(
    stats::runif(n_chains * n_prms, -b, b),
    nrow = n_prms, ncol = n_chains
  )

  proposal_mat <- prms_across_chains + gamma * (prms_m_mat - prms_n_mat) + noise

  # Evaluate proposals via batch version of `call_log_posterior_m`
  results <- call_log_posterior_m(
    proposal_mat = proposal_mat,
    prev_prms_mat = prms_across_chains,
    prev_pis = pis_across_chains,
    prev_lls = log_likes_across_chains,
    ...
  )

  # update the parameters on the chains which proposals were accepted
  # call_log_posterior_m() doesn't return the updated parameter matrix, just
  # the updated log-posteriors, and the log-likelihoods, so we have to
  # update the parameters here now.
  # Also update posteriors and log-likelihoods ... actually not necessary
  # but done for readability
  accepted <- results$accept
  prms_across_chains[, accepted] <- proposal_mat[, accepted]
  pis_across_chains[accepted] <- results$pis[accepted]
  log_likes_across_chains[accepted] <- results$lls[accepted]


  # wrap it up and return
  return_list = list(
    new_prms_across_chains = prms_across_chains,
    new_pis_across_chains = pis_across_chains,
    new_log_likes_across_chains = log_likes_across_chains
  )

  return(return_list)
}



#' Metropolis Acceptance Step for Vectorized MCMC Sampling
#'
#' This internal function computes the Metropolis acceptance decision
#' for a set of MCMC proposals, using either hierarchical or non-hierarchical
#' posterior evaluation. It returns updated log-posterior and log-likelihood
#' values, as well as a logical vector indicating accepted proposals.
#'
#' @param proposal_mat a numeric matrix of proposed parameter values.
#' Each column corresponds to one chain; rows represent parameters.
#' @param prev_prms_mat a numeric matrix of current (previous) parameter values.
#' Must have the same dimensions as `proposal_mat`.
#' @param prev_pis a numeric vector of current log-posterior values for each
#'  chain.
#' @param prev_lls a numeric vector of current log-likelihood values for each
#' chain.
#' @param level a character string specifying the sampling level, either
#' `"lower"`, `"hyper"`, or `"none"`. Determines whether to call
#' [dRiftDM::log_posterior_lower()] or [dRiftDM::log_posterior_hyper()].
#' @param re_eval logical. If `TRUE`, the log-posterior and log-likelihood for
#' the current parameters are re-evaluated.
#' @param ... Additional arguments passed to [dRiftDM::log_posterior_lower()] or
#' [dRiftDM::log_posterior_hyper()], depending on the `level`.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{`pis`}{A numeric vector of updated log-posterior values.}
#'   \item{`lls`}{A numeric vector of updated log-likelihood values.}
#'   \item{`accept`}{A logical vector of length equal to the number of chains,
#'   indicating which proposals were accepted.}
#' }
#'
#' @details
#' This function implements a vectorized Metropolis acceptance step for
#' multiple MCMC chains simultaneously. The posterior is calculated using
#' either [dRiftDM::log_posterior_lower()] for subject-level parameters or
#' [dRiftDM::log_posterior_hyper()] for group-level parameters.
#'
#' Log-posterior and log-likelihood values are only updated where proposals
#' were accepted. In cases where proposals yield invalid posteriors (i.e.,
#' `NA`), they are automatically rejected.
#'
#' @keywords internal
call_log_posterior_m <- function(proposal_mat, prev_prms_mat, prev_pis,
                                 prev_lls, level, re_eval,
                                 ...) {
  dotdot <- list(...)
  n_chains <- ncol(proposal_mat)

  # Re-evaluate log-posterior for previous params if needed
  # (only makes sense in the hierarchical case)
  if (re_eval && level != "none") {
    if (level == "lower") {
      prev_res <- log_posterior_lower(thetas_one_subj_mat = prev_prms_mat, ...)
    } else {
      prev_res <- log_posterior_hyper(phi_j_mat = prev_prms_mat, ...)
    }
    prev_pis <- prev_res$posterior_vals
    prev_lls <- prev_res$log_like_vals
  }

  # Compute log-posterior for proposals
  if (level == "lower" | level == "none") {
    prop_res <- log_posterior_lower(thetas_one_subj_mat = proposal_mat, ...)
  } else {
    prop_res <- log_posterior_hyper(phi_j_mat = proposal_mat, ...)
  }
  proposal_pis <- prop_res$posterior_vals
  proposal_lls <- prop_res$log_like_vals

  # Metropolis accept
  delta_log_post <- proposal_pis - prev_pis
  accept <- log(stats::runif(n_chains)) < delta_log_post
  accept[is.na(accept)] <- FALSE  # can happen for -Inf -(-Inf)

  # wrangle return (replace with proposal prob. if accepted)
  prev_pis[accept] = proposal_pis[accept]
  prev_lls[accept] = proposal_lls[accept]

  return_list = list(pis = prev_pis, lls = prev_lls, accept = accept)

  return(return_list)
}







#' Perform Crossover Between Chains
#'
#' This function dispatches to either [dRiftDM::full_crossover()] or
#' [dRiftDM::migration_crossover()] depending on the `which` argument.
#'
#' @inherit migration_crossover return
#'
#' @param which character string, Either `"diff"` or `"migration"`.
#' @param ... Further arguments passed to the underlying crossover function.
#'
#' @keywords internal
crossover = function(which, ...){

  which = match.arg(which, choices = c("diff", "migration"))

  if (which == "diff") {
    return(full_crossover(...))
  }

  return(migration_crossover(...))
}


# CUSTOM DISTRIBUTIONS ----------------------------------------------------



#' Truncated Normal Density Function
#'
#' Computes the probability density function for the truncated normal
#' distribution. This version supports both vector and matrix input for `x`.
#'
#' @param x A numeric vector or matrix of values where the density should be
#'  evaluated.
#' @param mean Mean of the normal distribution. Can be a scalar or vector
#'  (recycled if necessary).
#' @param sd Standard deviation of the normal distribution. Can be a scalar or
#'  vector (recycled if necessary).
#' @param lower Lower truncation bound. Can be a scalar or vector
#'  (recycled if necessary). Default is `-Inf`.
#' @param upper Upper truncation bound. Can be a scalar or vector
#'  (recycled if necessary). Default is `Inf`.
#' @param log Logical; if `TRUE`, probabilities `p` are given as `log(p)`.
#'  Default is `FALSE`.
#'
#' @return A numeric vector or matrix of the same shape as `x`, containing the
#' (possibly log) densities.
#'
#' @details
#' The function evaluates the normal density at `x` and scales it to reflect
#' truncation to the interval (`lower`, `upper`). Values outside the truncation
#' bounds are assigned a density of 0 (or `-Inf` on the log scale).
#' Internally, [stats::dnorm] and [stats::pnorm] are used.
#'
#' If `x` is a matrix, the result retains the same dimensions. All other
#' arguments are recycled as needed. For example, if x has two rows
#' and 5 columns, then mean might provide 2 values, so that the first/second row
#' is evaluated under the first/second mean value.
#'
#' @keywords internal
dtnorm <- function(x, mean = 0, sd = 1, lower = -Inf, upper = Inf,
                   log = FALSE) {
  # store original dimensions
  x_dim <- dim(x)
  x_vec <- as.vector(x)

  # init return vector
  d_vec <- if (log) rep(-Inf, length(x_vec)) else rep(0, length(x_vec))

  # out-of-bounds mask
  nas <- is.na(x_vec)
  out <- nas | x_vec > upper | x_vec < lower

  # numerator and denominator
  suppressWarnings({
    num <- stats::dnorm(x_vec[!out], mean = mean, sd = sd, log = log)
    denom <- stats::pnorm(upper, mean = mean, sd = sd) -
      stats::pnorm(lower, mean = mean, sd = sd)
  })


  # apply truncation adjustment
  if (log) {
    d_vec[!out] <- num - log(denom)
  } else {
    d_vec[!out] <- num / denom
  }

  # return NA for NA
  d_vec[nas] <- x_vec[nas]

  # restore dimensions (if any)
  if (!is.null(x_dim)) {
    d_vals <- matrix(d_vec, nrow = x_dim[1], ncol = x_dim[2])
  } else {
    d_vals <- d_vec
  }

  return(d_vals)
}


#' @rdname dtnorm
rtnorm <- function(n, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
  # get quantile levels for lower and upper
  F_lower <- stats::pnorm(q = lower, mean = mean, sd = sd)
  F_upper <- stats::pnorm(q = upper, mean = mean, sd = sd)

  # draw prob. values between the boundaries
  u <- stats::runif(n = n, min = F_lower, max = F_upper)

  # remap to quantile values
  values <- stats::qnorm(p = u, mean = mean, sd = sd)
  return(values)
}



# LOG-POSTERIORS ----------------------------------------------------------


drift_dm_supr_warn <- function(expr, suppress_warnings = TRUE) {
  if (suppress_warnings) suppressWarnings(expr) else expr
}


#' Conditional Log-Posterior Distributions for MCMC Sampling
#'
#' These functions compute conditional log-posterior distributions used in a
#' (hierarchical) MCMC sampler.
#'
#' `log_posterior_hyper()` computes the conditional log-posterior for a
#'  group-level hyperparameter matrix `phi_j_mat`, given
#'  the individual-level parameters across subjects `theta_j_mat` (for one
#'  type of model parameter).
#'
#' `log_posterior_lower()` computes the conditional log-posterior for
#'  an individual participant’s parameter matrix `thetas_one_subj_mat`, given
#'  prior distributions. In the hierarchical setting, the prior distributions
#'  are conditioned on the group-level parameters.
#'
#' @param phi_j_mat a numeric matrix of current group-level parameters
#'  for one individual-level parameter. It must be 2 x n_chains and provide the
#'  mean and standard deviation; in that order.
#' @param theta_j_mat a numeric matrix of individual-level parameter
#'  values across all individuals and chains for one model parameter. Must
#'  be n_chains x n_subj.
#' @param log_prior_hyper_fun a function that returns the log-prior density of
#'  the hyperparameters. Must be a single function (not a list of functions) and
#'  it must accept `phi_j_mat` as input.
#' @param log_prior_lower_fun a function that returns the log-prior density of
#'  individual parameter values given the mean and standard deviation at the
#'  group-level (as stored in `phi_j_mat`). Must be a single function
#'  (not a list of functions) and it must accept `theta_j_mat` as a first
#'  argument and input, and the mean and standard deviation with arguments
#'  `mean` and `sd` (vectorized).
#' @param thetas_one_subj_mat a named matrix of lower-level parameters for a
#'  single participant. Each row represents one parameter, and each column one
#'  chain.
#' @param all_phis_mat a named matrix of all current group-level parameters.
#'  Each mean group-level parameter must be named `"M-<param>"` and each
#'  standard deviation `"S-<param>"`. The `<param>` part must match the
#'  individual-level parameters in `thetas_one_subj_mat`. If this argument is
#'  `NULL`, this indicates that the estimation is done in a non-hierarchical
#'  fashion. Each row represents a hyper-parameter, and each column one
#'  chain.
#' @param model_subj a `drift_dm` object, containing an individual's data.
#' @param log_prior_lower_funs a named list of functions, one per parameter
#'  stored in `thetas_one_subj_mat`, returning the log-prior densities. It is
#'  assumed that each function can take one type of parameter across chains
#'  (i.e., a vector). In the non-hierarchical case, each function can only
#'  accept a single vector (for the respective individual-level parameter across
#'  chains). In the hierarchical case, each function must also support the
#'  arguments `mean` and `sd` for vectorized prior computation.
#' @param temperatures a numeric vector of temperature scaling values, one per
#'  chain, used when applying tempered inference (e.g., in TIDE).
#' @param suppress_warnings logical, if TRUE, warnings created from
#' `log_prior_hyper_fun` and `log_prior_lower_fun(s)` are suppressed. The
#' default is true, because in the beginning of an MCMC sampler implausible
#' proposals are provided which can yield missing values and warnings.
#'
#' @return A list with two elements:
#'  * `posterior_vals`, the total log-posterior values (log-likelihood +
#'  log-prior) per chain.
#'  * `log_like_vals`, the log-likelihood components only, per chain.
#'
#' @keywords internal
log_posterior_hyper <- function(phi_j_mat, theta_j_mat ,
                                log_prior_hyper_fun, log_prior_lower_fun,
                                temperatures, suppress_warnings = TRUE) {
  # get the log-density values of theta_j across all individuals (cols) and
  # chains (rows),
  # given one phi_j (rows) across chains (columns); phi_j contains the mean and
  # standard deviation at the group-level
  drift_dm_supr_warn({
    log_likes_mat <- log_prior_lower_fun(
      theta_j_mat,
      mean = phi_j_mat[1,],
      sd = phi_j_mat[2,]
    )
  }, suppress_warnings = suppress_warnings)
  stopifnot(dim(log_likes_mat) == dim(theta_j_mat))

  # log_likes has likelihoods for each individual (cols) across chains (rows)
  # -> sum across individuals, i.e., get sum for each chain.
  log_like_vals <- rowSums(log_likes_mat) * temperatures
  log_like_vals[is.na(log_like_vals)] <- -Inf

  # get the log prior density value for phi_j
  drift_dm_supr_warn({
     log_like_prior_vals <- log_prior_hyper_fun(phi_j_mat)
  }, suppress_warnings = suppress_warnings)
  stopifnot(is.vector(log_like_prior_vals))
  stopifnot(length(log_like_prior_vals) == dim(phi_j_mat)[2])

  # add everything together and pass back ....
  return_list <- list(
    posterior_vals = log_like_vals + log_like_prior_vals,
    log_like_vals = log_like_vals
  )
  return(return_list)
}


#' @rdname log_posterior_hyper
log_posterior_lower <- function(thetas_one_subj_mat, all_phis_mat,
                                model_subj, log_prior_lower_funs, temperatures,
                                suppress_warnings = TRUE) {
  # get the likelihoods of an individual's data given the model parameters
  # for this individual across chains
  # chains are assumed to be represented as cols, parameters as rows
  n_chains = ncol(thetas_one_subj_mat)

  log_like_vals = sapply(seq_len(n_chains), \(k) {
    model_subj$flex_prms_obj <- x2prms_vals(
      x = thetas_one_subj_mat[,k],
      flex_prms_obj = model_subj$flex_prms_obj
    )
    tryCatch(
      drift_dm_supr_warn(
        re_evaluate_model(model_subj)$cost_value * -1.0, # log-likelihood (not its negative)
        suppress_warnings = suppress_warnings
      ),
      error = function(e) {
        prms <- prms_to_str(model_subj)
        if (!suppress_warnings)
          warning("Evaluation of the model failed, tried values: \n", prms)
        return(NA_real_) # NA ensures that sapply returns a vector
      }
    )
  })
  stopifnot(is.vector(log_like_vals))
  stopifnot(length(log_like_vals) == n_chains)


  log_like_vals = log_like_vals * temperatures
  log_like_vals[is.na(log_like_vals)] <- -Inf

  # then get the log prior density values
  thetas_names <- rownames(thetas_one_subj_mat)
  if (!is.null(all_phis_mat)) {
    # this is the case for the hierarchical estimation, which assumes
    # that lower level parameters are distributed according to a normal
    # or truncated normal distribution with a mean and a standard deviation
    # given by the hyper-level parameters.
    Ms = paste("M", thetas_names, sep = "-")
    Ss = paste("S", thetas_names, sep = "-")
    phis_means_mat <- all_phis_mat[Ms, , drop = FALSE]
    phis_sds_mat <- all_phis_mat[Ss, , drop = FALSE]

    # iterate over each parameter
    log_priors_mat <- sapply(seq_along(thetas_names), function(theta_idx) {
      drift_dm_supr_warn({
        log_prior_lower_funs[[thetas_names[theta_idx]]]( # get the prior function
          thetas_one_subj_mat[theta_idx,],  # get the prm values across chains
          mean = phis_means_mat[theta_idx,], # get the mean across chains
          sd = phis_sds_mat[theta_idx,] # get the sds across chains
        )
      }, suppress_warnings = suppress_warnings)
    })
  } else {
    # this is the lower level case, which assumes that log_prior_lower_funs
    # just takes parameter values with the remaining parameters for
    # the respective distributions being fixed.
    log_priors_mat <- sapply(X = thetas_names, FUN = function(one_theta_name) {
      log_prior_lower_funs[[one_theta_name]](
        thetas_one_subj_mat[one_theta_name,]
      )
    })
  }
  # If n_chains > 1, the sapply call results in matrix with chains x parameters
  # -> sum over parameters; get sum for each chain
  # if n_chains = 1, then sapply returns a vector -> sum over parameters
  if (is.matrix(log_priors_mat)) {
    log_priors = rowSums(log_priors_mat)
  } else {
    log_priors = sum(log_priors_mat)
  }

  # combine everything and pass back ...
  return_list <- list(
    posterior_vals = log_like_vals + log_priors,
    log_like_vals = log_like_vals
  )
  return(return_list)
}



# PRIOR FUNCTIONS (DENSITIES AND RANDOM GENERATION) -----------------------


#' Default Prior for Group-Level (Hyper) Parameters
#'
#' These functions define and evaluate a default prior distribution for
#' hyperparameters at the group level.
#'
#' `d_default_prior_hyper` computes the (log) density of a prior for a
#' two-element vector or a 2xN matrix, containing the mean and standard
#' deviation (i.e., `phi_j`). The mean is modeled with a truncated
#' normal distribution, and the standard deviation with a gamma distribution.
#'
#' `r_default_prior_hyper` samples hyperparameter values from this prior.
#'
#' @param x a numeric vector of length 2 or a matrix with 2 rows and N column.
#'  Here, `x[1]` or `x[1,]` are interpreted as the group mean(s) and `x[2]` or
#'  `x[2,]` as the group standard deviation(s).
#' @param n number of samples to generate.
#' @param mean,sd mean and standard deviation of the truncated normal
#' distribution for the group-level mean. (recycled if necessary)
#' @param lower,upper lower and upper bounds for the truncated normal
#' distribution. (recycled if necessary)
#' @param shape,rate shape and rate parameters of the gamma distribution
#' for the group-level standard deviation. (recycled if necessary)
#' @param log logical; if `TRUE`, the log-density is returned.
#'
#' @return For `d_default_prior_hyper`, a numeric vector representing the
#' (log) prior density value(s), with the simplifying assumption of independence
#' of the mean and standard deviation.
#'
#' For `r_default_prior_hyper`, a 2-row matrix with `n` columns. The first
#' row contains sampled group means; the second row contains sampled
#' standard deviations. Samples are drawn independently. If `n` is 1, then
#' a named numeric vector is returned.
#'
#' @details
#' the arguments `mean`, `sd`, `lower`, `upper`, `shape`, and `rate` are
#' recycled if necessary with respect to the columns of `x`. For example,
#' if `x` has two columns, then `mean` might provide two values.
#'
#'
#' @keywords internal
d_default_prior_hyper <- function(x, mean, sd, lower, upper, shape, rate,
                                  log) {

  if (is.matrix(x)) {
    stopifnot(nrow(x) == 2)
    hyper_means = x[1,]
    hyper_sds = x[2,]
  } else {
    stopifnot(length(x) == 2)
    hyper_means = x[1]
    hyper_sds = x[2]
  }

  d_m <- dtnorm(
    x = hyper_means, mean = mean, sd = sd, lower = lower, upper = upper,
    log = log
  )
  d_sd <- stats::dgamma(x = hyper_sds, shape = shape, rate = rate, log = log)


  if (log) {
    vals <- d_m + d_sd
  } else {
    vals <- d_m * d_sd
  }
  return(vals)
}

#' @rdname d_default_prior_hyper
r_default_prior_hyper <- function(n, mean, sd, lower, upper, shape, rate) {
  stopifnot(n >= 1)
  r_m <- rtnorm(n = n, mean = mean, sd = sd, lower = lower, upper = upper)
  r_sd <- stats::rgamma(n = n, shape = shape, rate = rate)

  vals <- rbind(r_m, r_sd)
  if (n == 1) {
    vals <- stats::setNames(as.vector(vals), rownames(vals))
  }
  return(vals)
}



#' Generate Prior Functions for Model Parameters
#'
#' This function creates prior distribution functions for each model parameter
#' in a drift diffusion model (DDM), depending on the specified hierarchical
#' level. It returns both log-density functions and, where applicable,
#' random-sample generators based on the user-defined prior settings.
#'
#' @param drift_dm_obj a [dRiftDM::drift_dm] model object.
#' @param level a character string, specifying the modeling level. Must be one
#' of: `"hyper"` (group-level priors), `"lower"` (individual-level priors
#' given group-level parameters), or `"none"` (non-hierarchical setting).
#' @param means a named numeric vector or list, specifying the prior means for
#' each parameter. Missing values will be filled up from the first matching
#' parameter in `drift_dm_obj`
#' @param sds a named numeric vector or list of standard deviations. Missing or
#' `NULL` values will be replaced by corresponding values from `mean`.
#' @param lower,upper optional numeric vectors or lists specifying the lower
#' and upper truncation bounds for each prior distribution. Defaults to `-Inf`
#' and `Inf`, respectively.
#' @param shapes,rates optional numeric vectors or lists specifying the shape and
#' rate parameter for group-level standard deviations (used at the hyper-level).
#' Defaults to `1`.
#'
#' @return A named list with two elements:
#'  * `log_dens_priors`: A named list of functions. Each function returns the
#'     log-density for a parameter value, based on the chosen prior settings.
#'  * `r_priors`: A named list of functions for sampling from the specified
#'     prior distributions.
#'
#' @details
#' Each prior is parameter-specific and wrapped using [purrr::partial()] so
#' that downstream sampling or density evaluation can be performed easily.
#' At the hyper-level, the functions [dRiftDM::d_default_prior_hyper()] and
#' [dRiftDM::r_default_prior_hyper()] are used. At the lower-level, the
#' functions [dRiftDM::dtnorm()] and [dRiftDM::rtnorm()] are used.
#'
#' The input arguments `means`, `sds`, `lowers`, `uppers`, `shapes`, and `rates`
#' are handled by the function [dRiftDM::get_parameters_smart()].
#'
#' @seealso [dRiftDM::get_parameters_smart()], [dRiftDM::dtnorm()],
#' [dRiftDM::rtnorm()], [dRiftDM::d_default_prior_hyper()],
#' [dRiftDM::r_default_prior_hyper()]
#' @keywords internal
get_default_prior_settings <- function(drift_dm_obj, level, means = NULL,
                                       sds = NULL, lower = NULL, upper = NULL,
                                       shapes = NULL, rates = NULL) {
  ####
  # input handling and default settings
  # if mean is NULL, use the parameters of the model

  # enlarge to model parameters
  means <- get_parameters_smart(
    drift_dm_obj = drift_dm_obj,
    input_a = means,
    fill_up_with = NA_real_
  )$vec_a

  # if mean is NULL, set it equal to the model parameters, otherwise
  # replace missing values with the model parameters
  if (is.null(means)) {
    means = coef(drift_dm_obj)
  } else {
    means[is.na(means)] = coef(drift_dm_obj)[is.na(means)]
  }
  all_coefs <- names(means)

  # then, get the sds
  sds <- get_parameters_smart(
    drift_dm_obj = drift_dm_obj, input_a = sds,
    fill_up_with = NA_real_
  )$vec_a
  # if sds is NULL, set it equal to the means, otherwise replace missing values
  # with the remaining means
  if (is.null(sds)) {
    sds <- means
  } else {
    sds[is.na(sds)] <- means[is.na(sds)]
  }

  # do the same with lower, upper, shape, and rate; easier this time,
  # because the default value is always the same
  get_default <- function(input, default_value) {
    if (is.null(input)) {
      tmp <- rep(default_value, length(all_coefs))
      names(tmp) <- all_coefs
      return(tmp)
    }
    tmp <- get_parameters_smart(
      drift_dm_obj = drift_dm_obj, input_a = input, fill_up_with = default_value
    )$vec_a
    return(tmp)
  }

  lower <- get_default(lower, -Inf)
  upper <- get_default(upper, Inf)
  shapes <- get_default(shapes, 1)
  rates <- get_default(rates, 1)


  # level input match (three options: hyper-level priors, lower-level priors,
  # or none; none indicates a non-hierarchical setting)
  level <- match.arg(level, choices = c("hyper", "lower", "none"))


  ####
  # get the (log-)density functions, conditioned on the user-defined settings

  # helper function that calls purr::partial and sets arguments to fn
  create_partial_funs <- function(prms, fn, args_list) {
    sapply(prms, function(one_prm) {
      args <- lapply(args_list, `[[`, one_prm)
      do.call(purrr::partial, c(.f = fn, args))
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  log_scale <- rep(TRUE, length(all_coefs))
  names(log_scale) <- all_coefs

  # at the hyper-level: requires settings for mean,sd,lower,upper,shape,rate
  # because each phi_j is a tuple (see d_default_prior_hyper)
  if (level == "hyper") {
    log_dens_priors <- create_partial_funs(
      prms = all_coefs,
      fn = d_default_prior_hyper,
      args_list = list(
        mean = means, sd = sds, lower = lower, upper = upper, shape = shapes,
        rate = rates, log = log_scale
      )
    )
  }

  # at the lower-level: requires settings for lower,upper. lower-level
  # parameters are assumed to be normally distributed and the mean and sd
  # are provided at run time, given the hyper-level parameters of a chain
  if (level == "lower") {
    log_dens_priors <- create_partial_funs(
      prms = all_coefs,
      fn = dtnorm,
      args_list = list(lower = lower, upper = upper, log = log_scale)
    )
  }


  # in a non-hierarchical settings, mean, sd, lower, upper and log are required
  # for the priors of each parameter
  if (level == "none") {
    log_dens_priors <- create_partial_funs(
      prms = all_coefs,
      fn = dtnorm,
      args_list = list(
        mean = means, sd = sds, lower = lower, upper = upper,
        log = log_scale
      )
    )
  }


  ###
  # now get the functions for drawing random variables

  # hyper-level
  if (level == "hyper") {
    r_priors <- create_partial_funs(
      prms = all_coefs,
      fn = r_default_prior_hyper,
      args_list = list(
        mean = means, sd = sds, lower = lower,
        upper = upper, shape = shapes, rate = rates
      )
    )
  }

  # lower-level
  if (level == "lower") {
    r_priors <- create_partial_funs(
      prms = all_coefs,
      fn = rtnorm,
      args_list = list(lower = lower, upper = upper)
    )
  }

  # non-hierarchical setting
  if (level == "none") {
    r_priors <- create_partial_funs(
      prms = all_coefs,
      fn = rtnorm,
      args_list = list(mean = means, sd = sds, lower = lower, upper = upper)
    )
  }

  # pack up and return
  all_funs <- list(log_dens_priors = log_dens_priors, r_priors = r_priors)
  class(all_funs) <- "ddm_prior_settings"
  attr(all_funs, "level") <- level
  return(all_funs)
}




# ESTIMATION FUNCTIONS -----------------------------------------------------


#' (Hierarchical) Bayesian Estimation with Differential Evolution
#'
#' Estimate group-level and individual-level parameters with a
#' hierarchical Bayesian approach using Differential Evolution MCMC (DE-MCMC)
#' \insertCite{Turneretal.2013;textual}{dRiftDM}.
#' An approximation of the marginal likelihood to calculate Bayes Factors can
#' be obtained with the Thermodynamic Integration via Differential Evolution
#' (TIDE) sampler \insertCite{EvansAnnis2019;textual}{dRiftDM}.
#'
#'
#'
#' @param drift_dm_obj an object of type [dRiftDM::drift_dm].
#' @param obs_data_ids data.frame for the hierarchical case. An additional
#'  column ID is necessary that codes the individuals (see also
#'  [dRiftDM::obs_data]).
#' @param sampler character string, indicating the sampler to use.
#'  Must be either `"DE-MCMC"` (default) or `"TIDE"`.
#' @param n_chains numeric, number of chains for the MCMC-sampler.
#'  Default is `40`.
#' @param burn_in numeric, number of burn-in iterations. Default is `500`.
#' @param samples numeric, number of sampling iterations after burn-in.
#'  Default is `2000`.
#' @param n_cores numeric, number of threads to use for parallel processing in
#' the hierarchical case. Default is `1`.
#' @param prob_migration numeric, probability of performing a migration
#'  crossover step during burn-in. Default is `0.1` (i.e., 10%).
#' @param prob_re_eval numeric, probability of re-evaluating the
#'  likelihood/posterior values of the previous iteration `i-1` when deciding
#'  for the acceptance of the proposal in iteration `i`. Only considered during
#'  burn-in. Default is `0.1` (i.e., 10%).
#' @param verbose integer, indicating verbosity of output: 0 (none),
#'  1 (minimal text output), or 2 (text output and progress bar). Default is `2`.
#' @param seed optional random seed for reproducibility.
#' @param ... additional arguments passed to
#' [dRiftDM::get_default_prior_settings] to customize prior settings.
#'
#' @return A named ist containing posterior samples for
#'   group-level and individual-level parameters, log-posterior values,
#'   and log-likelihoods. Labels: `phi`, `pis_phi`, `lls_phi`, `theta`,
#'   `pis_theta`, `lls_theta`. The first three entries are only present in the
#'   hierarchical case.
#'
#'   The list also has an additional attribute named `data_model`. In the
#'   hierarchical case, the attribute contains a named list of model copies
#'   with all the individual data sets attached. The list is named according to
#'   the individual `ID`s in the argument `obs_data_ids`. In the
#'   non-hierarchical case, the attribute contains the model and its attached
#'   data.
#'
#' @details
#'
#' The function `estimate_bayes_h()` handles the hierarchical case. The function
#' `estimate_bayes_one_subj()` handles the case for estimating a single
#' individual. The reason for writing two functions is that the hierarchical
#' case has some unique tweaks to it that need to be considered ... and writing
#' one function would be quite the mess.
#'
#' Prior Settings: See the wrapper [dRiftDM::estimate_bayesian()] and
#' also [dRiftDM::get_default_prior_settings()]
#'
#' @references
#' \insertRef{Turneretal.2013}{dRiftDM}
#' \insertRef{EvansAnnis2019}{dRiftDM}
#'
#' @keywords internal
estimate_bayes_h <- function(drift_dm_obj, obs_data_ids, sampler, n_chains,
                             burn_in, samples, n_cores, prob_migration,
                             prob_re_eval, verbose, seed = NULL, ...) {

  sampler = match.arg(sampler, choices = c("DE-MCMC", "TIDE"))

  # wrangle data into a list of individual models
  data_model = lapply(unique(obs_data_ids$ID), \(x){
    obs_data(drift_dm_obj) = obs_data_ids[obs_data_ids$ID == x,]
    return(drift_dm_obj)
  })
  names(data_model) = unique(obs_data_ids$ID)

  # get the prior distributions for the hyper parameters phi
  prior_distr_hyper = get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "hyper", ...
  )
  log_priors_hyper = prior_distr_hyper$log_dens_priors
  r_priors_hyper = prior_distr_hyper$r_priors

  # get the prior distributions for the lower parameters theta
  prior_distr_lower = get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "lower", ...
  )
  log_priors_lower = prior_distr_lower$log_dens_priors
  r_priors_lower = prior_distr_lower$r_priors


  # names
  theta_names = names(log_priors_lower)
  phi_names = paste0(
    rep(c("M-", "S-"), length(log_priors_lower)),
    rep(theta_names, each = 2)
  )
  subj_names = names(data_model)

  # create containers for the parameters
  n_thetas = length(theta_names)
  n_phis = length(phi_names)
  n_subjs = length(data_model)
  iterations = as.integer(burn_in + samples + 1)
  n_chains = as.integer(n_chains)

  theta_array = array(
    0,
    dim = c(n_thetas, n_chains, n_subjs, iterations),
    dimnames = list(
      theta_names, seq_len(n_chains), subj_names, seq_len(iterations)
    )
  )
  phi_array = array(
    0,
    dim = c(n_phis, n_chains, iterations),
    dimnames = list(phi_names, seq_len(n_chains), seq_len(iterations))
  )

  # create containers for the log_posteriors and the log_likelihoods
  pis_theta = array(
    0,
    dim = c(n_chains, n_subjs, iterations),
    dimnames = list(seq_len(n_chains), subj_names, seq_len(iterations))
  )
  lls_theta = array(
    0,
    dim = c(n_chains, n_subjs, iterations),
    dimnames = list(seq_len(n_chains), subj_names, seq_len(iterations))
  )
  pis_phi = array(
    0,
    dim = c(n_thetas, n_chains, iterations),
    dimnames = list(theta_names, seq_len(n_chains), seq_len(iterations))
  )
  lls_phi = array(
    0,
    dim = c(n_thetas, n_chains, iterations),
    dimnames = list(theta_names, seq_len(n_chains), seq_len(iterations))
  )


  # create powers (or just get ones if not TIDE)
  temperatures = create_temperatures(n_chains, sampler)


  # turn on parallel engine
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterExport(
    cl,
    varlist = c("full_crossover", "call_log_posterior_m",
                "crossover", "migration_crossover", "log_posterior_lower",
                "re_evaluate_model", "prms_to_str",
                "x2prms_vals", "update_special_values",
                "drift_dm_default_rounding", "drift_dm_supr_warn",
                "data_model", "theta_names", "n_chains",
                "log_priors_lower", "r_priors_lower", "temperatures",
                "sampler"),
    envir = environment()
  )

  if (!is.null(seed)) {
    if (!is_numeric(seed) | length(seed) != 1)
      stop("seed must be a single numeric")
    parallel::clusterSetRNGStream(cl, iseed = seed)
  }

  # get starting values
  if (verbose >= 1) {
    message("Finding starting values...")
  }


  # draw random values for each hyper parameter
  for (one_prm in theta_names) {
    which_phis = paste0(c("M-", "S-"), one_prm)
    phi_array[which_phis, , 1] = r_priors_hyper[[one_prm]](n_chains)
  }


  # then draw thetas based on the provided mean priors (parallelized
  # because here I'll calculate the log-posterior as well)
  mean_hyper = rowMeans(phi_array[paste0("M-", theta_names), , 1])
  names(mean_hyper) = theta_names
  phi_start = phi_array[,,1]

  parallel::clusterExport(
    cl,
    varlist = c("phi_start", "mean_hyper"),
    envir = environment()
  )

  pis_lls_thetas <- parallel::parLapply(cl, subj_names, function(j) {

    # for each theta_j draw values across chains
    # results in a matrix n_chains x prms
    thetas_i <- sapply(theta_names, function(one_prm) {
      r_priors_lower[[one_prm]](
        n = n_chains,
        mean = mean_hyper[one_prm],
        sd = mean_hyper[one_prm] / 4
      )
    })

    pis_lls <- log_posterior_lower(
      thetas_one_subj_mat = t(thetas_i),
      all_phis_mat = phi_start,
      model_subj = data_model[[j]],
      log_prior_lower_funs = log_priors_lower,
      temperatures = temperatures
    )

    list(
      posterior_vals = pis_lls$posterior_vals,
      log_like_vals = pis_lls$log_like_vals,
      thetas_i = thetas_i
    )
  })


  pis_theta[, , 1] = sapply(pis_lls_thetas, \(x) x$posterior_vals)
  lls_theta[, , 1] = sapply(pis_lls_thetas, \(x) x$log_like_vals)

  # wrangle in the parameters (unlist and re-assemble; unlist
  # flattens by columns (i.e., each parameter across chains) then by list entries)
  thetas_tmp <- array(
    data = unlist(lapply(pis_lls_thetas, \(x) x[["thetas_i"]])),
    dim = c(n_chains, n_thetas, n_subjs)
  )

  # permute to get desired shape: n_chains x n_thetas x n_subjs
  thetas_tmp <- aperm(thetas_tmp, c(2, 1, 3))
  theta_array[,,, 1] = thetas_tmp


  # now with hopefully reasonable values for phi and theta, get pi for phi
  for (one_prm in theta_names) {
    which_phis = paste0(c("M-", "S-"), one_prm)
    posterior_ll_vals = log_posterior_hyper(
      phi_j_mat = phi_array[which_phis,  , 1],
      theta_j_mat = theta_array[one_prm, ,,1],
      log_prior_hyper_fun = log_priors_hyper[[one_prm]],
      log_prior_lower_fun = log_priors_lower[[one_prm]],
      temperatures = temperatures
    )
    pis_phi[one_prm,, 1] = posterior_ll_vals$posterior_vals
    lls_phi[one_prm,, 1] = posterior_ll_vals$log_like_vals
  }

  if (verbose >= 1) {
    message("Starting the sampling procedure")
  }

  if (verbose >= 2) {
    pb <- progress::progress_bar$new(
      format = "  sampling [:bar] :percent remaining: :eta",
      total = iterations - 1,
      clear = FALSE,
      width = 60
    )
  }

  # now the sampling...
  for (i in 2:iterations) {

    # determine which step to do (during burn_in do migration with some prob.)
    which = "diff"
    if ((i <= burn_in) && stats::runif(1) < prob_migration) {
      which = "migration"
    }

    # re-evaluate the posterior/likelihood with certain prob.
    re_eval = FALSE
    if (stats::runif(1) < prob_re_eval) {
      re_eval = TRUE
    }

    ####
    # update each phi across chains (separately)
    for (one_prm in theta_names) {
      which_phis = paste0(c("M-", "S-"), one_prm)

      # get previous phis and pis
      prev_prms_across_chains = phi_array[which_phis, , i - 1]
      prev_pis_across_chains = pis_phi[one_prm, , i - 1]
      prev_lls_across_chains = lls_phi[one_prm, , i - 1]

      # get the values for one theta across lower chains and subjs
      # if DE-MCMC (i.e., not TIDE), break dependence
      prev_thetas = theta_array[one_prm, , , i - 1]
      if (sampler == "DE-MCMC") {
        shuffle_idx = sample(seq_len(n_chains))
        prev_thetas = prev_thetas[shuffle_idx,]
      }

      # do the crossover
      returned_list = crossover(
        which = which, # which crossover function to call?
        prms_across_chains = prev_prms_across_chains, # prms across the chains
        pis_across_chains = prev_pis_across_chains,   # each pi per chain
        log_likes_across_chains = prev_lls_across_chains, # each ll per chain
        level = "hyper", # controls which log_posterior is called
        re_eval = re_eval, # re_evaluate at the hyper level
        theta_j_mat = prev_thetas, # necessary arguments
        log_prior_hyper_fun = log_priors_hyper[[one_prm]],
        log_prior_lower_fun = log_priors_lower[[one_prm]], # for the log_posterior
        temperatures = temperatures
      )

      # sort the values in
      phi_array[which_phis, , i] = returned_list$new_prms_across_chains
      pis_phi[one_prm, , i] = returned_list$new_pis
      lls_phi[one_prm, , i] = returned_list$new_log_likes
    }


    ####
    # now update the thetas and pis for each subject (because parallelized
    # this returns a list which we have to sort in again...)
    prev_prms_across_chains_subj = theta_array[, , , i - 1]
    prev_pis_across_chains_subj = pis_theta[, , i - 1]
    prev_log_likes_across_chains_subj = lls_theta[, , i - 1]
    cur_phis = phi_array[, ,i]


    parallel::clusterExport(
      cl,
      varlist = c("prev_prms_across_chains_subj", "prev_pis_across_chains_subj",
                  "prev_log_likes_across_chains_subj", "cur_phis",
                  "re_eval", "which"),
      envir = environment()
    )

    # TODO: Reduce the Overhead
    new_thetas_and_pis <- parallel::parLapply(cl, subj_names, function(j) {

      # get previous thetas and pis
      prev_prms_across_chains <- prev_prms_across_chains_subj[, ,j]
      prev_pis_across_chains <- prev_pis_across_chains_subj[, j]
      prev_lls_across_chains <- prev_log_likes_across_chains_subj[, j]

      # shuffle the upper chains to break dependence
      cur_phis_local <- cur_phis
      if (sampler == "DE-MCMC") {
        shuffle_idx <- sample(seq_len(n_chains))
        cur_phis_local <- cur_phis_local[, shuffle_idx]
      }

      # do the crossover
      returned_list <- crossover(
        which = which,
        prms_across_chains = prev_prms_across_chains,
        pis_across_chains = prev_pis_across_chains,
        log_likes_across_chains = prev_lls_across_chains,
        level = "lower",
        re_eval = re_eval,
        all_phis_mat = cur_phis_local,
        model_subj = data_model[[j]],
        log_prior_lower_funs = log_priors_lower,
        temperatures = temperatures
      )

      return(returned_list)
    })
    names(new_thetas_and_pis) = subj_names

    # sort the values in
    for (j in subj_names) {
      theta_array[, ,j , i] = new_thetas_and_pis[[j]]$new_prms_across_chains
      pis_theta[,j , i] = new_thetas_and_pis[[j]]$new_pis
      lls_theta[,j , i] = new_thetas_and_pis[[j]]$new_log_likes
    }

    if (verbose >= 2) pb$tick()
  }

  parallel::stopCluster(cl)

  # drop the burn_in period
  idx_after_burn_in = (burn_in + 2):iterations
  phi_array = phi_array[,,idx_after_burn_in, drop = FALSE]
  pis_phi = pis_phi[,,idx_after_burn_in, drop = FALSE]
  lls_phi = lls_phi[,,idx_after_burn_in, drop = FALSE]

  theta_array = theta_array[,,,idx_after_burn_in, drop = FALSE]
  pis_theta = pis_theta[,,idx_after_burn_in, drop = FALSE]
  lls_theta = lls_theta[,,idx_after_burn_in, drop = FALSE]

  # wrap up everything in a list and pass back
  r_l = list(phi = phi_array, pis_phi = pis_phi, lls_phi = lls_phi,
             theta = theta_array, pis_theta = pis_theta, lls_theta = lls_theta)
  attr(r_l, "data_model") <- data_model
  return(r_l)
}




#' @rdname estimate_bayes_h
#' @keywords internal
estimate_bayes_one_subj <- function(drift_dm_obj, sampler, n_chains,
                                    burn_in, samples, prob_migration,
                                    prob_re_eval, verbose, ...) {


  # get the prior distributions for theta
  prior_distr = get_default_prior_settings(
    drift_dm_obj = drift_dm_obj,
    level = "none", ...
  )
  log_priors = prior_distr$log_dens_priors
  r_priors = prior_distr$r_priors


  # names and containers
  theta_names = names(log_priors)
  n_thetas = length(theta_names)
  iterations = as.integer(burn_in + samples + 1)
  n_chains = as.integer(n_chains)

  theta_array = array(
    0,
    dim = c(n_thetas, n_chains, iterations),
    dimnames = list(
      theta_names, seq_len(n_chains), seq_len(iterations)
    )
  )

  # create containers for the log_posteriors and the log_likelihoods
  pis_theta = array(
    0,
    dim = c(n_chains, iterations),
    dimnames = list(seq_len(n_chains), seq_len(iterations))
  )
  lls_theta = array(
    0,
    dim = c(n_chains, iterations),
    dimnames = list(seq_len(n_chains), seq_len(iterations))
  )

  # create powers (or get ones)
  temperatures = create_temperatures(n_chains, sampler)


  # get starting values
  if (verbose >= 1) {
    message("Finding starting values...")
  }

  # results in a matrix n_chains x prms
  thetas_i <- sapply(theta_names, \(one_prm) r_priors[[one_prm]](n = n_chains))
  thetas_i <- t(thetas_i)

  # get the log_likelihoods and pis
  pis_lls <- log_posterior_lower(
    thetas_one_subj_mat = thetas_i,
    all_phis_mat = NULL,
    model_subj = drift_dm_obj,
    log_prior_lower_funs = log_priors,
    temperatures = temperatures
  )

  # sort the values in
  pis_theta[, 1] = pis_lls$posterior_vals
  lls_theta[, 1] = pis_lls$log_like_vals
  theta_array[,, 1] = thetas_i


  # start the sampling....
  if (verbose >= 1) {
    message("Starting the sampling procedure")
  }

  if (verbose >= 2) {
    pb <- progress::progress_bar$new(
      format = "  sampling [:bar] :percent remaining: :eta",
      total = iterations - 1,
      clear = FALSE,
      width = 60
    )
  }


  for (i in 2:iterations) {

    # determine which step to do (during burn_in do migration with some prob.)
    which = "diff"
    if ((i <= burn_in) && stats::runif(1) < prob_migration) {
      which = "migration"
    }

    # re-evaluate the posterior/likelihood with certain prob.
    re_eval = FALSE
    if (stats::runif(1) < prob_re_eval) {
      re_eval = TRUE
    }

    # do the crossover
    returned_list <- crossover(
      which = which,
      prms_across_chains = theta_array[,,i-1],
      pis_across_chains = pis_theta[,i-1],
      log_likes_across_chains = lls_theta[,i-1],
      level = "none",
      re_eval = re_eval,
      all_phis_mat = NULL,
      model_subj = drift_dm_obj,
      log_prior_lower_funs = log_priors,
      temperatures = temperatures
    )

    # sort the values in
    theta_array[, , i] = returned_list$new_prms_across_chains
    pis_theta[, i] = returned_list$new_pis
    lls_theta[, i] = returned_list$new_log_likes
    if (verbose >= 2) pb$tick()
  }

  # drop the burn_in period
  idx_after_burn_in = (burn_in + 2):iterations
  theta_array = theta_array[,,idx_after_burn_in, drop = FALSE]
  pis_theta = pis_theta[,idx_after_burn_in, drop = FALSE]
  lls_theta = lls_theta[,idx_after_burn_in, drop = FALSE]


  # wrap up everything
  r_l = list(theta = theta_array, pis_theta = pis_theta, lls_theta = lls_theta)
  attr(r_l, "data_model") <- drift_dm_obj
  return(r_l)
}


#' (Hierarchical) Bayesian Estimation
#'
#' This function provides a wrapper around the implemented samplers for
#' Bayesian inference in dRiftDM. For parameter estimation,
#' Differential Evolution Markov-Chain Monte-Carlo (DE-MCMC)
#' \insertCite{Turneretal.2013;textual}{dRiftDM} is used.
#' An approximation of the marginal likelihood to calculate Bayes Factors can
#' be obtained with the Thermodynamic Integration via Differential Evolution
#' (TIDE) sampler \insertCite{EvansAnnis2019;textual}{dRiftDM}. However,
#' TIDE is not yet supported fully, and is at an experimental stage.
#'
#' @inheritParams estimate_bayes_h
#' @param ... additional arguments passed forward to
#' [dRiftDM::estimate_bayes_h()] and [dRiftDM::estimate_bayes_one_subj()].
#'
#' @returns an object of type `mcmc_dm`  containing posterior samples for
#'   parameters, log-posterior values, and log-likelihoods. In the hierarchical
#'   case, the respective values are available at both the group-level and the
#'   individual-level. The object contains two attributes: `sampler` and
#'   `data_model`. The former simply stores the type of sampler that was used
#'   and codes whether estimation was done in a hierarchical fashion or not.
#'   The latter either contains the model and the attached data (in the
#'   non-hierarchical case) or a named list of model copies with each
#'   individual's data attached.
#'
#' @details
#' When a [data.frame] is supplied, a hierarchical approach to parameter
#' estimation is done. In this case, the supplied data set must provide data
#' for multiple individuals. To estimate the parameters for a single individual
#' (i.e., pursue the non-hierarchical approach), then the supplied model
#' `drift_dm_obj` must have data attached to it (see [dRiftDM::obs_data()]).
#'
#'
#' @references
#' \insertRef{Turneretal.2013}{dRiftDM}
#' \insertRef{EvansAnnis2019}{dRiftDM}
#'
#'
#'
#' @seealso [dRiftDM::summary.mcmc_dm()], [dRiftDM::estimate_bayes_h()],
#' [dRiftDM::estimate_bayes_one_subj()]
#' @keywords internal
estimate_bayesian = function(drift_dm_obj, obs_data_ids = NULL,
                             sampler, n_chains, burn_in, samples,
                             prob_migration, prob_re_eval,
                             verbose = NULL, ...) {



  # input checks (no checks on drift_dm_obj and obs_data_ids,
  # these have to be supplied with reasonable values from estimate_dm())
  sampler = match.arg(sampler, c("DE-MCMC", "TIDE"))

  if (!is_numeric(n_chains) | n_chains < 3) {
    stop("n_chains must be a numeric >= 3")
  }
  if (!is_numeric(burn_in) | burn_in < 0) {
    stop("burn_in must be a numeric >= 0")
  }
  if (!is_numeric(samples) | samples < 1) {
    stop("samples must be a numeric >= 1")
  }
  if (!is_numeric(prob_migration) |
      !(prob_migration >= 0 & prob_migration <= 1)) {
    stop("prob_migration must be a numeric between 0 and 1")
  }
  if (!is_numeric(prob_re_eval) |
      !(prob_re_eval >= 0 & prob_re_eval <= 1)) {
    stop("prob_re_eval must be a numeric between 0 and 1")
  }
  if (is.null(verbose)) {
    verbose = 2
  }
  if (!is_numeric(verbose) | !(verbose %in% 0:2)) {
    stop("verbose must be either 0, 1, or 2")
  }
  if (cost_function(drift_dm_obj) != "neg_log_like") {
    stop("cost function of the model is not the negative log-likelihood!")
  }


  # decide over dispatch
  if (!is.null(obs_data_ids)) {
    hierarchical = TRUE # controls if the model is estimated hierarchically
    results = estimate_bayes_h(
      drift_dm_obj = drift_dm_obj, obs_data_ids = obs_data_ids,
      sampler = sampler,
      n_chains = n_chains, burn_in = burn_in, samples = samples,
      prob_migration = prob_migration, prob_re_eval = prob_re_eval,
      verbose = verbose, ...
    )
  } else {
    hierarchical = FALSE
    results = estimate_bayes_one_subj(
      drift_dm_obj = drift_dm_obj, sampler = sampler,
      n_chains = n_chains, burn_in = burn_in, samples = samples,
      prob_migration = prob_migration, prob_re_eval = prob_re_eval,
      verbose = verbose, ...
    )
  }
  # result is a list of chains, containing parameters, posteriors, and
  # log_likelihoods, with the the attribute "data_model".


  # calculate the marginal distribution
  ti = NA_real_
  if (sampler == "TIDE") {
    temperatures = create_temperatures(n_chains, sampler)
    lls_theta = results$lls_theta
    m_ll_theta = apply(lls_theta, MARGIN = 1, mean)
    ti = sum(diff(temperatures) /
               2 * (utils::tail(m_ll_theta,-1) + utils::head(m_ll_theta,-1)))
  }

  # add ti value to the list
  results$ti = ti

  # give it a class label and attribute containing the sampler
  class(results) <- "mcmc_dm"
  attr(results, "sampler") = sampler
  attr(results, "hierarchical") = hierarchical

  # and pass back
  return(results)
}



#' Create "Temperatures" for TIDE
#'
#' @param n_chains numeric
#' @param sampler "TIDE" or anything else
#'
#' @returns a numeric vector of length equal to `n_chains`. The returned values
#' correspond to quantiles of a Beta(0.3, 1) distribution for
#' `sampler == "TIDE"`. Otherwise a numeric vector of `1`s is returned.
#'
#' @keywords internal
create_temperatures = function(n_chains, sampler){
  stopifnot(is.integer(n_chains))
  stopifnot(length(n_chains) == 1)

  sampler = match.arg(sampler, choices = c("TIDE", "DE-MCMC"))
  alpha = 0.3
  temperatures = rep(1, n_chains)
  if (sampler == "TIDE") {
    temperatures = c(0:(n_chains - 1) / (n_chains - 1))^(1 / alpha)
  }
  return(temperatures)
}



# GENERAL HELPER FUNCTIONS ------------------------------------------------


#' Extract a Subset of MCMC Chains
#'
#' When calling [dRiftDM::estimate_bayesian()], the MCMC results are
#' packed up as an `mcmc_dm` object. This function is used in the depths of
#' `dRiftDM` to extract the relevant array of MCMC samples,
#' depending on whether the model is hierarchical and whether a participant ID
#' is provided.
#'
#' @param chains_obj an object of class`mcmc_dm`.
#' @param id an optional single numeric or character, specifying the `ID` of a
#' participant to extract individual-level samples from a hierarchical model.
#' Ignored for non-hierarchical models.
#'
#' @return A 3D array of MCMC samples. The first dimension indicates parameters,
#'  the second dimension chains, and the third dimension iterations
#' @keywords internal
get_subset_chains <- function(chains_obj, id = NULL) {

  stopifnot(inherits(chains_obj, "mcmc_dm"))
  if (!is.null(id)) {
    stopifnot(length(id) == 1)
    stopifnot(is.character(id) | is_numeric(id))
  }

  hierarchical <- attr(chains_obj, "hierarchical")
  if (hierarchical & !is.null(id)) {
    chains <- chains_obj[["theta"]]  # prms x chains x subjs x iterations
    all_ids <- dimnames(chains)[[3]]
    id = as.character(id)
    if (!(id %in% all_ids)) {
      stop("ID ", id, " was not found")
    }
    which_id <- which(id == all_ids)
    chains <- chains[, , which_id, ]
  } else if (hierarchical & is.null(id)) {
    chains <- chains_obj[["phi"]]
  } else {
    chains <- chains_obj[["theta"]]
  }

  stopifnot(!is.null(dim(chains)))
  stopifnot(length(dim(chains)) == 3)
  return(chains)
}


#' Convert MCMC Chain Array to a `coda::mcmc.list` Object
#'
#' Converts a 3D MCMC chain array (parameters × chains × iterations)
#' into a `coda::mcmc.list` object for compatibility with diagnostic and
#' summary functions from the `coda` package.
#'
#' @param chains a 3D numeric array with dimensions corresponding to
#' parameters × chains × iterations. `chains` are typically obtained from a call
#' to [dRiftDM::get_subset_chains()].
#'
#' @return An object of class `mcmc.list` containing one `mcmc` object per chain.
#'
#' @seealso [coda::mcmc()], [coda::mcmc.list()]
#' @keywords internal
mcmc_dm_to_coda_mcmc <- function(chains) {

  stopifnot(length(dim(chains)) == 3)
  n_chains = dim(chains)[2]

  list_of_chains = lapply(seq_len(n_chains), \(x){
    subset = chains[, x, ] # extract chain
    # if chains only contains one parameter dimension are dropped, and might
    # result in a vector instead of an array/matrix
    if (is.vector(subset)) {
      subset = matrix(subset, ncol = 1)
      colnames(subset) = dimnames(chains)[[1]]
    } else {
      subset = t(subset)
    }
    coda::mcmc(subset)
  })

  coda_mcmc_obj = do.call(coda::mcmc.list, args = list_of_chains)

  return(coda_mcmc_obj)
}

