% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_estimate.R
\name{estimate_model}
\alias{estimate_model}
\title{Estimate the Parameters of a drift_dm Model}
\usage{
estimate_model(
  drift_dm_obj,
  lower,
  upper,
  verbose = 0,
  use_de_optim = TRUE,
  use_nmkb = FALSE,
  seed = NULL,
  de_n_cores = 1,
  de_control = list(reltol = 1e-08, steptol = 50, itermax = 200, trace = FALSE),
  nmkb_control = list(tol = 1e-06)
)
}
\arguments{
\item{drift_dm_obj}{an object inheriting from \link{drift_dm}}

\item{lower, upper}{numeric vectors or lists, specifying the lower and upper
bounds on each parameter to be optimized (see Details).}

\item{verbose}{numeric, indicating the amount of information displayed.
If 0, no information is displayed (default). If 1, basic information about
the start of Differential Evolution or Nelder-Mead and the final
estimation result is given. If 2, each evaluation of the log-likelihood
function is shown. Note that \code{verbose} is independent of the information
displayed by \link[DEoptim:DEoptim]{DEoptim::DEoptim}.}

\item{use_de_optim}{logical, indicating whether Differential Evolution via
\link[DEoptim:DEoptim]{DEoptim::DEoptim} should be used. Default is \code{TRUE}}

\item{use_nmkb}{logical, indicating whether Nelder-Mead via
\link[dfoptim:nmkb]{dfoptim::nmkb} should be used. Default is \code{FALSE}.}

\item{seed}{a single numeric, providing a seed for the Differential Evolution
algorithm}

\item{de_n_cores}{a single numeric, indicating the number of cores to use.
Run \code{\link[parallel:detectCores]{parallel::detectCores()}} to see how many cores are available on your
machine. Note that it is generally not recommended to use all of your cores
as this will drastically slow down your machine for any additional task.}

\item{de_control, nmkb_control}{lists of additional control parameters passed
to \link[DEoptim:DEoptim]{DEoptim::DEoptim} and \link[dfoptim:nmkb]{dfoptim::nmkb}.}
}
\value{
the updated \code{drift_dm_obj} (with the estimated parameter values,
log-likelihood, and probability density functions of the first passage time)
}
\description{
Find the 'best' parameter settings by fitting a \link{drift_dm} models'
predicted probability density functions (PDFs) to the observed data
stored within the respective object. The fitting procedure is done by
minimizing the negative log-likelihood of the model.

Users have three options:
\itemize{
\item Estimate the parameters via Differential Evolution (Default)
\item Estimate the parameters via (bounded) Nelder-Mead
\item Use Differential Evolution followed by Nelder-Mead.
}

See also \code{vignette("dRiftDM", "dRiftDM")}
}
\details{
\subsection{Specifying lower/upper}{

the function \code{estimate_model} provides a flexible way of specifying the
search space; identical to specifying the parameter simulation space in
\link{simulate_data.drift_dm}.

Users have three options to specify the simulation space:
\itemize{
\item Plain numeric vectors (not very much recommended). In this case,
\code{lower/upper} must be sorted in accordance with the parameters in the
\code{flex_prms_obj} object that vary for at least one condition
(call \code{print(drift_dm_obj)} and have a look at the \verb{Unique Parameters}
output)
\item Named numeric vectors. In this case \code{lower/upper} have to provide labels
in accordance with the parameters that are considered "free" at least once
across conditions.
\item The most flexible way is when \code{lower/upper} are lists. In this case, the
list requires an entry called "default_values" which specifies the named or
plain numeric vectors as above. If the list only contains this entry, then
the behavior is as if \code{lower/upper} were already numeric vectors. However,
the \code{lower/upper} lists can also provide entries labeled as specific
conditions, which contain named (!) numeric vectors with parameter labels.
This will modify the value for the upper/lower parameter space with respect
to the specified parameters in the respective condition.
}
}

\subsection{Details on Nelder-Mead and Differential Evolution}{

If both \code{use_de_optim} and \code{use_nmkb} are \code{TRUE}, then Nelder-Mead follows
Differential Evolution. Note that Nelder-Mead requires a set of starting
parameters for which either the parameter values of \code{drift_dm_obj} or the
estimated parameter values by Differential Evolution are used.

Default settings will lead \link[DEoptim:DEoptim]{DEoptim::DEoptim} to stop if the algorithm is
unable to reduce the negative log-likelihood by a factor of
\code{reltol * (abs(val) + reltol)}after \code{steptol = 50} steps, with
\code{reltol = 1e-8} (or if the default itermax of 200 steps is reached).
Similarly, \link[dfoptim:nmkb]{dfoptim::nmkb} will stop if the absolute difference of the
log-likelihood between successive iterations is below \code{tol = 1e-6}.See
\link[DEoptim:DEoptim.control]{DEoptim::DEoptim.control} and the details of \link[dfoptim:nmkb]{dfoptim::nmkb} for
further information.
}
}
\examples{

# the example uses a simple model and the Nelder-Mead minimization
# routine to ensure that it runs in a couple of seconds.

# get a model and attach data to the model
my_model <- ratcliff_dm(t_max = 1.5, dx = .005, dt = .005)
obs_data(my_model) <- ratcliff_synth_data # this data set comes with dRiftDM


# set the search space
lower <- c(muc = 1, b = 0.2, non_dec = 0.1)
upper <- c(muc = 7, b = 1.0, non_dec = 0.6)

# then fit the data to the model using Nelder-Mead after setting some start
# values
coef(my_model) <- c(muc = 2, b = 0.5, non_dec = 0.4)
my_model <- estimate_model(
  drift_dm_obj = my_model, # (starting values are those set to the model)
  lower = lower, # lower and upper parameter ranges
  upper = upper,
  use_de_optim = FALSE, # don't use the default diff. evol. algorithm
  use_nmkb = TRUE # but Nelder-Mead (faster, but way less robust)
)

# show the result
print(my_model)

}
\seealso{
\link{estimate_model_ids}
}
