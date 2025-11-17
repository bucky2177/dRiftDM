# checks if all entries of internal_list are an expression or integer. Throws an error if not (internal docu)

Checks also if there are entries for each parameter and condition

## Usage

``` r
check_internal_list(internal_list, prm_names, cond_names)
```

## Arguments

- internal_list:

  a list, referring to a (linear) internal list of a
  [flex_prms](https://bucky2177.github.io/dRiftDM/reference/flex_prms.md)
  object.

- prm_names:

  the expected parameter of the flex_prms object

- cond_names:

  the expected conditions of the flex_prms object

## Value

the internal list for convenience
