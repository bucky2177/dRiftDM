# Extract the conditions and parameters from an instruction string

This function takes an instruction as a string and then extracts the
conditions and parameters, depending on what "type" of operation it is.
The expected structure is listed in the details docu of flex_prms()

## Usage

``` r
prms_conds_to_modify(formula_instr, operation, all_conds, all_prms)
```

## Arguments

- formula_instr:

  an instruction string

- operation:

  what to expect in terms of the string's structure. Can be "vary",
  "restrain", "fix", "set" or "dependency"

- all_conds:

  all potential conditions (necessary for extending missing condition
  specification)

- all_prms:

  all potential paramters (necessary for extending missing condition
  specification)

## Value

a named list prms_to_adress and conds_to_adress as character vectors.
