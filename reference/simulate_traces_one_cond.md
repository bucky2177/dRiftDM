# Simulate Traces for One Conditions

The function simulates traces with forward Euler. It is the backend
function to `simulate_traces`.

## Usage

``` r
simulate_traces_one_cond(drift_dm_obj, k, one_cond, add_x, sigma)
```

## Arguments

- drift_dm_obj:

  a model of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- k:

  a single numeric, the number of traces to simulate

- one_cond:

  a single character string, specifying which condition shall be
  simulated

- add_x:

  a single logical, indicating if starting values shall be added or not.
  Sometimes, when visualizing the model, one does not want to have the
  starting values.

- sigma:

  a single numeric, to override the "sigma" in
  [prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md)

## Value

An array of size k times `nt + 1`. The array becomes an object of type
`traces_dm`, which allows for easier printing with
[print.traces_dm](https://bucky2177.github.io/dRiftDM/reference/simulate_traces.md).
Furthermore, each object has the additional attributes:

- "t_vec" -\> the time space from 0 to t_max

- "mu_vals" -\> the drift rate values by mu_fun

- "b_vals" -\> the boundary values by b_fun

- "samp_x" -\> the values of the starting points (which are always added
  to the traces in the array.

- "add_x" -\> boolean, indicating if the starting values were added or
  not

- "orig_model_class" -\> the class label of the original model

- "orig_prms" -\> the parameters with which the traces were simulated
  (for the respective condition)

- "b_coding" -\> the boundary coding

- "prms_solve" -\> the solver settings with which the traces were
  simulated
