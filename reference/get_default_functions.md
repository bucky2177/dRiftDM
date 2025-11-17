# Get default/fall back component functions

If arguments are provided that are not NULL, the respective argument is
simply returned. If it is NULL, then a default/fall back component
function is returned for the respective component. This function is
called to fill up non-specified component functions when calling
[`drift_dm()`](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md).

## Usage

``` r
get_default_functions(
  mu_fun = NULL,
  mu_int_fun = NULL,
  x_fun = NULL,
  b_fun = NULL,
  dt_b_fun = NULL,
  nt_fun = NULL
)
```

## Arguments

- mu_fun:

  drift rate function

- mu_int_fun:

  integral drift rate function

- x_fun:

  starting point function

- b_fun:

  boundary function

- dt_b_fun:

  derivative of boundary function

- nt_fun:

  non-decision time function

## Value

a list of `mu_fun`, `mu_int_fun`, `x_fun`, `b_fun`, `dt_b_fun`, and
`nt_fun`, with either the supplied component functions or the
added/filled in default component functions (if an argument is NULL).

## Details

defaults...

- mu_fun -\> constant drift rate of 3 (i.e., vector of 0s)

- mu_int_fun -\> constant drift rate of 3 (i.e., vector of 3 times
  t_vec)

- x_fun -\> dirac delta on zero
  [`x_dirac_0()`](https://bucky2177.github.io/dRiftDM/reference/x_dirac_0.md)

- b_fun -\> constant boundary of 0.5 (i.e., vector of 0.5s)

- dt_b_fun -\> derivate of constant boundary (i.e., vector of 0s).
  [`dt_b_constant()`](https://bucky2177.github.io/dRiftDM/reference/dt_b_constant.md)

- nt_fun -\> constant non-decision time of 0.3 (i.e., vector for dirac
  delta on 0.5).
