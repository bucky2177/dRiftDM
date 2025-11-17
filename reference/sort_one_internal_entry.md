# Sorts the numbers in ascending order

.... within one entry (i.e., one prm across all conditions, such as when
calling `internal_list[["A"]]`); which is a list. Used to ensure that
parameter ordering remains logical

## Usage

``` r
sort_one_internal_entry(one_internal_entry)
```

## Arguments

- one_internal_entry:

  one entry of a (linearized) internal_list, must be named

## Value

the newly sorted entry as a list

## Details

The entries of the internal list are either digits (0-x) or expressions.
