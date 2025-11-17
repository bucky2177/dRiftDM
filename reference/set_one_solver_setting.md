# Set one specific aspect of the solver settings

Internal function to update one aspect of `prms_solve` or `solver`.

## Usage

``` r
set_one_solver_setting(drift_dm_obj, name_prm_solve, value_prm_solve)
```

## Arguments

- drift_dm_obj:

  an object of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- name_prm_solve:

  which aspect to address? ("sigma", "t_max", "dx", "dt", "solver")

- value_prm_solve:

  either a single numeric or character string

## Value

the updated un-evaluated (!) drift_dm_obj object

## Details

Ensures that the supplied values are reasonable and that `nx` and `nt`
are updated. The functions prms_solve\<- and solver\<- pass their
arguments forward to this function.
