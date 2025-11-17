# Check and Reduce the Observed Data

Checks a data set that is considered an "observed data set". Used in the
internals of dRiftDM. When calling this function, unncessary column
names are stripped away.

## Usage

``` r
check_reduce_raw_data(obs_data, b_coding_column, u_value, l_value)
```

## Arguments

- obs_data:

  a [data.frame](https://rdrr.io/r/base/data.frame.html)

- b_coding_column:

  a single string, indicating which column of `obs_data` indicates how
  each RT corresponds to the boundaries.

- u_value, l_value:

  the value within the `b_coding_column` column that specifies the
  upper/lower boundary

## Value

the `obs_data` for convenience (with edits as listed under Details).

## Details

Checks:

- if `obs_data` is a data.frame

- if "RT", `b_coding_column`, and "Cond" column are present

- when IDs are present, if each ID has values on each condition. At the
  same time unused factor levels are dropped
  [drop_levels_ID_column](https://bucky2177.github.io/dRiftDM/reference/drop_levels_ID_column.md)

- If all columns are there, the data set is reduced to the relevant ones

- for missing Values, and drops rows with missing values

- if "Cond" is of type character, and if not casts it to character

- if RT is of type numeric, and of not casts it to numeric

- RTs are \>= 0

- that the values in `b_coding_column` match with u_value and l_value
  (casts the column if necessary)

- if `b_coding_column` has only 1 or 2 unique values
