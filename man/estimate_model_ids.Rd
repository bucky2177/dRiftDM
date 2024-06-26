% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_estimate_wrapper.R
\name{estimate_model_ids}
\alias{estimate_model_ids}
\title{Fit multiple individuals and save results}
\usage{
estimate_model_ids(
  drift_dm_obj,
  obs_data_ids,
  lower,
  upper,
  fit_procedure_name,
  folder_name = fit_procedure_name,
  seed = NULL,
  fit_dir = "drift_dm_fits",
  force_refit = FALSE,
  progress = 2,
  start_vals = NULL,
  ...
)
}
\arguments{
\item{drift_dm_obj}{an object inheriting from \link{drift_dm} that will
be estimated for each individual in \code{obs_data_ids}.}

\item{obs_data_ids}{data.frame, must be suitable for
\link{set_obs_data}. An additional column \code{ID} necessary, to
identify a single individual}

\item{lower, upper}{numeric vectors, providing the parameter space, see
\link{estimate_model}.}

\item{fit_procedure_name}{character, providing a name of the fitting
procedure. This name will be stored in \code{drift_dm_fit_info.rds} to identify
the fitting procedure, see also \link{load_fits_ids}.}

\item{folder_name}{character, a folder name for storing all the individual
model fits. This variable should just state the name, and should not be
a path. Per default \code{folder_name} is identical to \code{fit_procedure_name}.}

\item{seed}{numeric, a seed to make the fitting procedure reproducable
(only relevant for differntial evolution, see \link{estimate_model}).
Default is \code{NULL} which means no seed.}

\item{fit_dir}{character, a directory where (multiple) fitting procedures
can be stored. If the directory does not exist yet, it will be created
via \code{base::create.dir(fit_dir, recursive = TRUE)}. Default is
\code{"drift_dm_fits"}.}

\item{force_refit}{logical, if \code{TRUE} each individual of a fitting routine will
be fitted once more. Default is \code{FALSE} which indicates that saved files}

\item{progress}{numerical, indicating if and how progress shall be depicted.
If 0, no progress is shown. If 1, the currently fitted individual is printed
out. If 2, a progressbar is shown. Default is 2.}

\item{start_vals}{optional data.frame, providing values to be set
before calling \link{estimate_model}. Can be used to control the
starting values for each individual when calling Nelder-Mead. Note that this
will only have an effect if DEoptim is not used (i.e., when setting
\code{use_de_optim = FALSE}; see \link{estimate_model}). The data.frame
must provide a column \code{ID} whose entries match the \code{ID} column
in \code{obs_data_ids}, as well as a column for each parameter of the model.}

\item{...}{additional arguments passed down to \link{estimate_model}.}
}
\description{
Provides a wrapper around \link{estimate_model} to fit multiple
individuals. Each individual will be stored in a folder. This folder will
also contain a file \code{drift_dm_fit_info.rds}, containing the main arguments
of the function call.
}
\details{
Examples and more information can be found here
\code{vignette("use_ddm_models", "dRiftDM")}.

When developing the fitting routine we had three levels of files/folders
in mind:
\itemize{
\item In a directory/folder named \code{fit_dir} multiple fitting routines can be
stored
\item Each fitting routine has its own folder with name as given by
\code{folder_name}
\item Within each folder, a file called
\code{drift_dm_fit_info.rds} contains the main information about the function
call. That is, the time when last modifying/calling a fitting routine, the
\code{lower} and \code{upper} parameter boundaries, the \code{drift_dm_object} that was
fitted to each individual, the original data set \code{obs_data_ids}, and
the identifier \code{fit_procedure_name}. In the same folder each individual
has its own \verb{<individual>.rds} file containing the modified \code{drift_dm_object}.
}
}
