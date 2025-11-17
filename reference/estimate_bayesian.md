# (Hierarchical) Bayesian Estimation

This function provides a wrapper around the implemented samplers for
Bayesian inference in dRiftDM. For parameter estimation, Differential
Evolution Markov-Chain Monte-Carlo (DE-MCMC) Turner et al. (2013) is
used. An approximation of the marginal likelihood to calculate Bayes
Factors can be obtained with the Thermodynamic Integration via
Differential Evolution (TIDE) sampler Evans and Annis (2019) . However,
TIDE is not yet supported fully, and is at an experimental stage.

## Usage

``` r
estimate_bayesian(
  drift_dm_obj,
  obs_data_ids = NULL,
  sampler,
  n_chains,
  burn_in,
  samples,
  prob_migration,
  prob_re_eval,
  verbose = NULL,
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

- ...:

  additional arguments passed forward to
  [`estimate_bayes_h()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayes_h.md)
  and
  [`estimate_bayes_one_subj()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayes_h.md).

## Value

an object of type `mcmc_dm` containing posterior samples for parameters,
log-posterior values, and log-likelihoods. In the hierarchical case, the
respective values are available at both the group-level and the
individual-level. The object contains two attributes: `sampler` and
`data_model`. The former simply stores the type of sampler that was used
and codes whether estimation was done in a hierarchical fashion or not.
The latter either contains the model and the attached data (in the
non-hierarchical case) or a named list of model copies with each
individual's data attached.

## Details

When a [data.frame](https://rdrr.io/r/base/data.frame.html) is supplied,
a hierarchical approach to parameter estimation is done. In this case,
the supplied data set must provide data for multiple individuals. To
estimate the parameters for a single individual (i.e., pursue the
non-hierarchical approach), then the supplied model `drift_dm_obj` must
have data attached to it (see
[`obs_data()`](https://bucky2177.github.io/dRiftDM/reference/obs_data.md)).

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

## See also

[`summary.mcmc_dm()`](https://bucky2177.github.io/dRiftDM/reference/summary.mcmc_dm.md),
[`estimate_bayes_h()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayes_h.md),
[`estimate_bayes_one_subj()`](https://bucky2177.github.io/dRiftDM/reference/estimate_bayes_h.md)
