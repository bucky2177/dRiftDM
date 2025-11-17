# (Hierarchical) Bayesian Estimation with Differential Evolution

Estimate group-level and individual-level parameters with a hierarchical
Bayesian approach using Differential Evolution MCMC (DE-MCMC) Turner et
al. (2013) . An approximation of the marginal likelihood to calculate
Bayes Factors can be obtained with the Thermodynamic Integration via
Differential Evolution (TIDE) sampler Evans and Annis (2019) .

## Usage

``` r
estimate_bayes_h(
  drift_dm_obj,
  obs_data_ids,
  sampler,
  n_chains,
  burn_in,
  samples,
  n_cores,
  prob_migration,
  prob_re_eval,
  verbose,
  seed = NULL,
  ...
)

estimate_bayes_one_subj(
  drift_dm_obj,
  sampler,
  n_chains,
  burn_in,
  samples,
  prob_migration,
  prob_re_eval,
  verbose,
  ...
)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

- obs_data_ids:

  data.frame for the hierarchical case. An additional column ID is
  necessary that codes the individuals (see also
  [obs_data](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)).

- sampler:

  character string, indicating the sampler to use. Must be either
  `"DE-MCMC"` (default) or `"TIDE"`.

- n_chains:

  numeric, number of chains for the MCMC-sampler. Default is `40`.

- burn_in:

  numeric, number of burn-in iterations. Default is `500`.

- samples:

  numeric, number of sampling iterations after burn-in. Default is
  `2000`.

- n_cores:

  numeric, number of threads to use for parallel processing in the
  hierarchical case. Default is `1`.

- prob_migration:

  numeric, probability of performing a migration crossover step during
  burn-in. Default is `0.1` (i.e., 10%).

- prob_re_eval:

  numeric, probability of re-evaluating the likelihood/posterior values
  of the previous iteration `i-1` when deciding for the acceptance of
  the proposal in iteration `i`. Only considered during burn-in. Default
  is `0.1` (i.e., 10%).

- verbose:

  integer, indicating verbosity of output: 0 (none), 1 (minimal text
  output), or 2 (text output and progress bar). Default is `2`.

- seed:

  optional random seed for reproducibility.

- ...:

  additional arguments passed to
  [get_default_prior_settings](https://bucky2177.github.io/dRiftDM/reference/get_default_prior_settings.md)
  to customize prior settings.

## Value

A named ist containing posterior samples for group-level and
individual-level parameters, log-posterior values, and log-likelihoods.
Labels: `phi`, `pis_phi`, `lls_phi`, `theta`, `pis_theta`, `lls_theta`.
The first three entries are only present in the hierarchical case.

The list also has an additional attribute named `data_model`. In the
hierarchical case, the attribute contains a named list of model copies
with all the individual data sets attached. The list is named according
to the individual `ID`s in the argument `obs_data_ids`. In the
non-hierarchical case, the attribute contains the model and its attached
data.

## Details

The function `estimate_bayes_h()` handles the hierarchical case. The
function `estimate_bayes_one_subj()` handles the case for estimating a
single individual. The reason for writing two functions is that the
hierarchical case has some unique tweaks to it that need to be
considered ... and writing one function would be quite the mess.

Prior Settings: See the wrapper
[`estimate_bayesian()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayesian.md)
and also
[`get_default_prior_settings()`](https://bucky2177.github.io/dRiftDM/reference/get_default_prior_settings.md)

## References

Turner BM, Sederberg PB, Brown SD, Steyvers M (2013). “A method for
efficiently sampling from distributions with correlated dimensions.”
*Psychological Methods*, **18**(3), 368–384.
[doi:10.1037/a0032222](https://doi.org/10.1037/a0032222) . Evans NJ,
Annis J (2019). “Thermodynamic integration via differential evolution: A
method for estimating marginal likelihoods.” *Behavior Research
Methods*, **51**, 930–947.
[doi:10.3758/s13428-018-1172-y](https://doi.org/10.3758/s13428-018-1172-y)
.
