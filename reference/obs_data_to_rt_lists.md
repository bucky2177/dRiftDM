# Disassemble an Observed Data set

Takes a data.frame with columns RT, Cond, and `column` matching with
[b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md),
and disassembles it into a list of rts

## Usage

``` r
obs_data_to_rt_lists(obs_data, b_coding = NULL)
```

## Arguments

- obs_data:

  a data.frame wth columns RT, Cond, and `column` matching `b_coding`

- b_coding:

  a boundary coding list (see
  [b_coding](https://bucky2177.github.io/dRiftDM/reference/b_coding.md))

## Value

a list of rts with entries

- rts_u -\> containing a list of numeric vectors, with names according
  to the values in Cond

- rts_l -\> containing a list of numeric vectors, with names according
  to the values in Cond

## Details

performs checks on `b_coding`
([check_b_coding](https://bucky2177.github.io/dRiftDM/reference/check_b_coding.md))
and checks/reduces `obs_data`
([check_reduce_raw_data](https://bucky2177.github.io/dRiftDM/reference/check_reduce_raw_data.md))
before disassembling the data set.
