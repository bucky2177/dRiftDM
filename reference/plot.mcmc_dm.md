# Plot MCMC Results and Diagnostics for `mcmc_dm` Objects

Visualize MCMC results and diagnostics for `mcmc_dm` objects. The
function `plot.mcmc()` is typically called when users supply an
`mcmc_dm` object returned by
[`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md)
to the generic [`base::plot()`](https://rdrr.io/r/base/plot.html)
function.

## Usage

``` r
# S3 method for class 'mcmc_dm'
plot(x, ..., id = NULL, what = "trace", bundle_plots = TRUE)
```

## Arguments

- x:

  an object of class `mcmc_dm`, as returned by
  [`estimate_dm()`](https://bucky2177.github.io/dRiftDM/reference/estimate_dm.md).

- ...:

  optional arguments passed on to the underlying plotting functions
  [`plot_mcmc_trace()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md),
  [`plot_mcmc_marginal()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md),
  and
  [`plot_mcmc_auto()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md).
  See the respective documentations for a list of optional arguments and
  the examples below. Probably the most relevant optional argument is
  `which_prms` that allows users to select a specific subset of
  parameters.

- id:

  optional character vector, specifying the id(s) of participants to
  plot. If `length(id) > 1`, `plot.mcmc_dm()` is called recursively,
  iterating over each entry in `id`. Each `id` must match with the
  relevant dimension names of the used chains array stored in `x`.

- what:

  a character string indicating the type of plot to produce. Must be
  either `"trace"`, `"density"`, or `"auto"`. See the Details below.
  Default is `"trace"`.

- bundle_plots:

  logical, indicating whether to display separate panels in a single
  plot layout (`FALSE`), or to plot them separately (`TRUE`).

## Value

Returns `NULL` invisibly.

## Details

This function provides diagnostic and summary visualizations of MCMC
samples. It handles results from both hierarchical and non-hierarchical
MCMC runs:

- If `id` is provided, the plot refers to the requested participant,
  with MCMC results extracted at the individual level.

- If `id` is omitted, plots refer to group-level parameters (i.e., the
  hyperparameters)

The following plot types are supported:

- Trace plots (`what = "trace"`): These plots show sampled parameter
  values across MCMC iterations for each chain. They are primarily used
  to inspect convergence and mixing behavior. Ideally, all chains should
  appear well-mixed (i.e., they should overlap and sample in a similar
  range). Lack of convergence is indicated by chains that remain in
  separate regions or exhibit trends over time.

- Density plots (`what = "density"`): These plots display smoothed
  marginal posterior distributions for each parameter, collapsed over
  chains and iterations. They are useful for understanding the central
  tendency, variance, and shape of the posterior distributions.

- Autocorrelation plots (`what = "auto"`): These plots display the
  autocorrelation at different lags, averaged across chains. They are
  useful to judge how quickly the chains produced independent samples.

## See also

[`plot_mcmc_trace()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md),
[`plot_mcmc_marginal()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md),
[`plot_mcmc_auto()`](https://bucky2177.github.io/dRiftDM/reference/plot_mcmc_trace.md)

## Examples

``` r
# get an examplary `mcmc_dm` object
chains_obj <- get_example_fits("mcmc")
plot(chains_obj)

plot(chains_obj, what = "density")

plot(chains_obj, what = "density", which_prm = "b", bundle_plots = FALSE)

```
