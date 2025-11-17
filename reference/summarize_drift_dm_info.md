# Reads Info file

Requires a path to the info file of a fit procedure (see
[estimate_model_ids](https://bucky2177.github.io/dRiftDM/reference/estimate_model_ids.md))
and turns its main information into a string

## Usage

``` r
summarize_drift_dm_info(full_name_to_file, detailed_info)
```

## Arguments

- full_name_to_file:

  path to the info file (.RDS)

- detailed_info:

  logical, if detailed info shall be provided or not

## Value

a string with infos about the fit procedure name, last call, and, if
detailed_info = TRUE, model, individuals, lower/upper, and seed.
