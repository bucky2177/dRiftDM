# Check a B_Coding

Checks if a list satisfies the requirements to be considered a valid
[b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md)

## Usage

``` r
check_b_coding(b_coding)
```

## Arguments

- b_coding:

  a named list

## Value

the unmodified list for convenience

## Details

Checks for...

- input being a list

- list is of length three and provides the names "column",
  "u_name_value", "l_name_value",

- if b_coding\$column provides a single string

- if b_coding\$u_name_value and b_coding\$l_name_value provides a single
  named value of type character or numeric, and that both are of the
  same type
