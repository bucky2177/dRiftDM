# Plot MCMC Chains for Drift Diffusion Model Parameters

The functions provide visualizations of MCMC results obtained for
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)
objects. Users won't call the functions directly. Instead, they are
called via
[`plot.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/plot.mcmc_dm.md),
and the following documentation helps to define optional arguments that
can be passed on via `...`.

## Usage

``` r
plot_mcmc_trace(
  chains,
  col_palette = grDevices::rainbow,
  col_chains = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL
)

plot_mcmc_marginal(
  chains,
  col_line = NULL,
  col_shade = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL
)

plot_mcmc_auto(
  chains,
  lags = 1:30,
  col_line = NULL,
  which_prms = NULL,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  type = NULL,
  main = NULL
)
```

## Arguments

- chains:

  an array of MCMC samples with three dimensions (parameters × chains ×
  iterations; see also
  [`plot.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/plot.mcmc_dm.md)).
  This argument is not optional and will be provided by
  [`plot.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/plot.mcmc_dm.md).

- col_palette:

  a function to generate a color palette for chains (default is
  grDevices::rainbow). Must be callable like this `col_palette(n)`.

- col_chains:

  a character vector, defining colors(s) to override `col_palette` for
  chain lines. Can be a single value or a vector matching the number of
  chains. Recycled if necessary to match the number of chains.

- which_prms:

  a regular expression (string), used to select a subset of parameters
  to plot. For example, `"^v"` would match all parameters starting with
  `v`. See also the examples in
  [`plot.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/plot.mcmc_dm.md).

- xlab, ylab:

  character vector(s), specifying axis labels for each panel. Recycled
  if necessary to match the number of plots being generated.

- xlim, ylim:

  list(s), containing length-2 numeric vectors specifying x- and y-axis
  limits, respectively. Recycled if necessary to match the number of
  plots being generated.

- col_line:

  a character vector, defining color(s) for the line outlines in the
  density plots. Can be a single value or a vector matching the number
  of parameters to plot. Recycled if necessary to match the number of
  parameters being plotted.

- col_shade:

  a character vector, defining color(s) for the shaded areas in the
  density plots. Can be a single value or a vector matching the number
  of parameters to plot. Recycled if necessary to match the number of
  parameters being plotted.

- lags:

  a numeric vector, giving the lags at which to calculate
  autocorrelation (see also
  [`coda::autocorr.diag()`](https://rdrr.io/pkg/coda/man/autocorr.diag.html)
  and [`coda::autocorr()`](https://rdrr.io/pkg/coda/man/autocorr.html)).

- type:

  a character, specifying how autocorrelations shall be displayed.
  Defaults to "h" (see the `type` argument of
  [`base::plot()`](https://rdrr.io/r/base/plot.html)). Recycled if
  necessary to match the number of parameters being plotted.

- main:

  character vector, specifying the labels above each autocorrelation
  plot. Defaults to parameter labels. Recycled if necessary to match the
  number of parameters being plotted.

## Value

These functions are called for their side effects (producing plots).
They return `NULL` invisibly.

## Details

- `plot_mcmc_trace()` plots one panel per parameter, with lines for each
  chain showing how values evolve over iterations. This is useful for
  diagnosing convergence and mixing.

- `plot_mcmc_marginal()` plots smoothed marginal posterior densities
  collapsed over chains and iterations for each parameter, useful for
  inspecting posterior distributions.

- `plot_mcmc_auto()` plots the autocorrelation among samples as function
  of `lags`.

## See also

[`plot.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/plot.mcmc_dm.md)
