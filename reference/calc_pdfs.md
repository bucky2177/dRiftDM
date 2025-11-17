# Calculate the PDFs

This method takes a model, the time and space vectors, and the unpacked
parameters for solving the PDF to derive the first passage time across
all conditions. It is a wrapper around the cpp implementations and
[add_residual](https://bucky2177.github.io/dRiftDM/reference/add_residual.md).
Important: This function is used in the depths of the package and the
generic method is not exported.

## Usage

``` r
calc_pdfs(drift_dm_obj, x_vec, t_vec, prms_solve)

# S3 method for class 'drift_dm'
calc_pdfs(drift_dm_obj, x_vec, t_vec, prms_solve)
```

## Arguments

- drift_dm_obj:

  a model of type
  [drift_dm](https://bucky2177.github.io/dRiftDM/reference/drift_dm.md)

- x_vec:

  numeric vector, the evidence space

- t_vec:

  numeric vector, the time space

- prms_solve:

  the discretization (see
  [prms_solve](https://bucky2177.github.io/dRiftDM/reference/prms_solve.md))

## Value

a list of PDFs, with named entries for each condition. Each of this
entry contains a list of vectors, named "pdf_u" and "pdf_l"

## Details

calc_pdfs is a generic method which dispatches the function call (not
exported). Currently, the method only considers objects of type
drift_dm.

calc_pdfs.drift_dm is the function that will be called for all models.
It evaluates the different components of a model, and subsequently calls
the cpp implementations for the KFE or integral method. It also calls
the
[add_residual](https://bucky2177.github.io/dRiftDM/reference/add_residual.md)
function to convolute the non-decision time to the first passage time.

The numerical methods for deriving the PDFs are based on the code
provided by (Richter et al. 2023) .

## References

Richter T, Ulrich R, Janczyk M (2023). “Diffusion models with
time-dependent parameters: An analysis of computational effort and
accuracy of different numerical methods.” *Journal of Mathematical
Psychology*, **114**, 102756.
[doi:10.1016/j.jmp.2023.102756](https://doi.org/10.1016/j.jmp.2023.102756)
.

## See also

[add_residual](https://bucky2177.github.io/dRiftDM/reference/add_residual.md)
