# Helper to get, set, or reset package-global options for statistics

Internal utility to manage global options for the package via the
"stats.dRiftDM" option slot.

## Usage

``` r
stats.options(...)
```

## Arguments

- ...:

  input, see Details below

## Value

Depending on usage:

- Full list of options (if no input),

- A specific option value (if string input),

- Invisibly `NULL` (if setting or resetting options).

## Details

Usage patterns:

- `stats.options()`: Returns the full list of currently stored options.

- `stats.options(name)`: Returns the value of a specific option (must be
  a single unnamed string).

- `stats.options(name = value, ...)`: Sets (or updates) named option(s).

- `stats.options(NULL)`: Resets (clears) the entire option list.

This function is intended for internal use only. It behaves similarly to
[`options()`](https://rdrr.io/r/base/options.html) and keeps all
package-specific options in a single named list under
`getOption("stats.dRiftDM")`.

Setting an argument can only be done once with this function, any
additional attempts to modify an option will not work (unless this
argument is explicitly set to `NULL`).
