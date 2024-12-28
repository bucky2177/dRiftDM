## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## Resubmission
This is a resubmission. In this version (0.2.1) I have:

* omitted the redundant "in R" at the end of your title.

* removed unnecessary spaces in the description field.

* omitted examples for unexported functions (is_numeric)

* removed all dontrun statements
  -> for estimate_model, there is now only one example which runs quickly
  -> estimate_model_ids, there is now only one example which runs quickly
  -> load_fits_ids, I have placed a fit procedure in the inst folder of the 
  package, which is now loaded in the example.
  
* ensured that only 2 cores will be used in examples and tests

* ensured that functions do not write by default to user's filespace (users now
  have to specify the argument fit_path when calling estimate_model_ids)

