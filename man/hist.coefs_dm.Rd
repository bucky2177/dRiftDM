% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{hist.coefs_dm}
\alias{hist.coefs_dm}
\title{Plot Parameter Distribution(s)}
\usage{
\method{hist}{coefs_dm}(
  x,
  ...,
  separate_plots = TRUE,
  alpha = 0.5,
  main = NULL,
  colors = NULL,
  xlab = "values"
)
}
\arguments{
\item{x}{an object of class \code{coefs_dm} (see \link{coef.fits_ids_dm})}

\item{...}{additional arguments passed to the \link[graphics:hist]{graphics::hist} function.}

\item{separate_plots}{logical, indicating whether to display separate panels
for each parameter in a single plot layout (\code{TRUE}), or to plot them
sequentially (\code{FALSE}).}

\item{alpha}{numeric, specifying the transparency level for histogram colors
when conditions are present, with values between 0 (fully transparent) and
1 (fully opaque).}

\item{main}{character vector, specifying titles for each parameter histogram.
Defaults to parameter names.}

\item{colors}{character vector, specifying colors for each condition if
conditions are present. Defaults to a rainbow color palette.
If \code{NULL} and no conditions are present, the default color is \code{"skyblue"}.}

\item{xlab}{character, specifying the label for the x-axis.}
}
\value{
Nothing (\code{NULL}; invisibly)
}
\description{
This function creates a histogram for each parameter in a \code{coefs_dm} object,
resulting from a call to \link{coef.fits_ids_dm}.
}
\details{
The \code{hist.coefs_dm} function is designed for visualizing parameter
distributions for a single fit procedure.

If multiple conditions are present, it overlays histograms for each condition
with adjustable transparency.

When \code{separate_plots} is set to \code{TRUE}, histograms for each parameter are
displayed in a grid layout within a single graphics device.
}
\examples{
# get an auxiliary fit procedure result (see the function load_fits_ids)
all_fits <- get_example_fits_ids()
hist(coef(all_fits)) # only three participants in this fit_ids object

# allows for some customization
hist(coef(all_fits), colors = "lightgreen")

}
