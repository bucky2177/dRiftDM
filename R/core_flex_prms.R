

# FUNCTION FOR CREATING FLEX_PRMS -----------------------------------------

#' Flex_Prms
#'
#' Functions for creating, accessing replacing, or printing a `flex_prms` object.
#' Any object of type `flex_prms` provides a user-friendly way to specify
#' dependencies, parameter values etc. for a model.
#'
#'
#' @param object an `R` object (see Details)
#' @param ... additional arguments passed on to the specific method.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when replacing the `flex_prms` object (see [dRiftDM::re_evaluate_model]).
#' @param conds A character vector, giving the names of the model's conditions.
#'  values within `conds` will be used when addressing the data and when
#'  deriving the model's predictions.
#' @param instr optional string with "instructions", see
#'   [dRiftDM::modify_flex_prms()].
#'
#' @param messaging optional logical, indicates if messages shall be ushered
#'  when processing `instr`.
#' @param round_digits integer, controls the number of digits shown when
#'  printing out a `flex_prms` object. Default is `3`.
#' @param dependencies  logical, controlling if a summary of the special
#' dependencies shall be printed.
#' @param cust_parameters logical, controlling if a summary of the custom
#' parameters shall be printed.
#' @param value an object of type `flex_prms`.
#' @param x an object of type `flex_prms`
#'
#'
#'
#' @returns The specific value returned depends on which method is called
#'
#' ## Creating an object of type `flex_prms`
#'
#'  Can be achieved by calling `flex_prms()` with a named numeric vector, thus
#'  when calling the underlying method `flex_prms.numeric` (see the example
#'  below). In this case a list with the class label `"flex_prms"` is returned.
#'  It containts three entries:
#'
#'  * A nested list `internal_list`. This list specifies the dependencies
#' and restrains enforced upon the parameters across conditions. Integers >= 1
#' indicate that this parameter will be estimated for a specific condition, and
#' conditions with the same number refer to a single parameter. Integers == 0
#' indicate thtat this parameter will not be esitmated for a specific condition
#' (i.e., it is considered "fixed"). Expressions will be evaluated at run time
#' and specify special dependencies among parameters.
#'
#'  * A nested list `linear_internal_list`. This list essentially contains
#' the same information as `internal_list`, but the parameters are sorted so
#' that they can be mapped to an integer vector (relevant only in the depths of
#' the package for the minimization routines).
#'
#'  * A numeric matrix `prms_matrix` which contains the currently set values for
#' each parameter across all conditions. Per default, the values of each
#' parameter are set equal across all conditions. Additionally, each parameter
#' is assumed to be restrained as equal across all conditions.
#' The values for all parameters given a condition will be passed to the
#' component functions (see [dRiftDM::comp_funs]).
#'
#'  * (optional) A list of additional parameters `cust_prms` that are derived
#' from the parameters in `prms_matrix`.
#'
#'
#' ## Accessing an object of type `flex_prms`
#'
#'  Users can access/get the `flex_prms` object when calling `flex_prms()` with
#'  an object of type [dRiftDM::drift_dm], `fits_ids_dm`
#'  (see [dRiftDM::estimate_model_ids()]), or `flex_prms`. In this case, the
#'  stored `flex_prms` object is returned.
#'
#' ## Replacing an object of type `flex_prms`
#'
#' The `flex_prms` object stored within an object of type [dRiftDM::drift_dm]
#' can be replaced by calling the generic `flex_prms<-` replacement function. In
#' this case, the modified [dRiftDM::drift_dm] object is returned.
#'
#' @details
#'
#' Objects of type `flex_prms` can be modified using the generic
#' [dRiftDM::modify_flex_prms()] function and a corresponding set of
#' "instructions" (see the respective function for more details).
#'
#' `flex_prms()` is a generic function. If called with a named numeric
#' vector, then this will create an object of type `flex_prms` (requires
#' `conds` to be specified). If called with other data types, gives the
#' respective `flex_prms` object
#'
#' `flex_prms<-()` is a generic replacement function. Currently this only
#' supports objects of type [dRiftDM::drift_dm]. It will replace/update the
#' model with a new instance of type `flex_prms`.
#'
#' @note
#' There is only a replacement function for [dRiftDM::drift_dm] objects. This is
#' because replacing the solver settings after the model has been fitted (i.e.,
#' for a `fits_ids_dm` object) doesn't make sense.
#'
#' @seealso [dRiftDM::estimate_model_ids()], [dRiftDM::drift_dm()],
#' [dRiftDM::summary.flex_prms()], [dRiftDM::modify_flex_prms()]
#'
#' @examples
#' conds <- c("foo", "bar")
#' prms <- c(a = 3, b = 4)
#' one_instr <- "a ~ foo + bar"
#' flex_prms_obj <- flex_prms(prms, conds = conds,
#'                            instr = one_instr)
#'
#' @export
flex_prms <- function(object, ...) {
  UseMethod("flex_prms")
}


#' @rdname flex_prms
#' @export
flex_prms.numeric = function(object, ..., conds, instr = NULL,
                             messaging = NULL) {

  prms_model <- object

  # input checks
  if (length(prms_model) == 0) {
    stop("supplied prms_model (object) has length 0")
  }
  check_if_named_numeric_vector(x = prms_model, var_name = "prms_model (object)")
  if (!is.character(conds) | length(conds) == 0) {
    stop("conds is not a character vector of length >= 1")
  }

  if (any(grepl('\\W', conds, perl = T))){
    stop("some condition name contain illegal non-alphanumeric characters")
  }


  # extract the parmaeter values and the parameter names
  prms_vals = unname(prms_model)
  name_prms_model = names(prms_model)


  # create the internal_list
  internal_list <- vector("list", length(name_prms_model))
  names(internal_list) <- name_prms_model
  internal_list = lapply(internal_list, function(x){
    one_internal_entry = as.list(rep(1L, length(conds)))
    names(one_internal_entry) = conds
    return(one_internal_entry)
  })


  # create the linearized internal_list
  linear_internal_list = linearize_internal_list(internal_list)


  # create the matrix of all prms
  prms_vals_matrix = matrix(rep(prms_vals, length(conds)),
                            nrow = length(conds), byrow = TRUE)
  rownames(prms_vals_matrix) = conds
  colnames(prms_vals_matrix) = name_prms_model


  # assemble everything
  flex_prms_obj <- list(internal_list = internal_list,
                        linear_internal_list = linear_internal_list,
                        prms_matrix = prms_vals_matrix)

  class(flex_prms_obj) <- "flex_prms"


  # run optional instructions
  flex_prms_obj = modify_flex_prms(object = flex_prms_obj, instr = instr,
                                   messaging = messaging)


  # and pass back
  return(flex_prms_obj)
}



# FUNCTION FOR INJECTING A PARAMETER VECTOR -------------------------------


#' Update the parameter matrix for vector inputs (internal docu)
#'
#' This function takes a numeric vector and maps the values to the parameter
#' matrix using the linearized internal list. This will also lead to an update
#' of the values for which special dependencies were set
#'
#' @param x a numeric vector with new values to set
#' @param flex_prms_obj a [dRiftDM::flex_prms] with the (linearized) internal
#' list and the parameter matrix
#' @return a flex_prms_obj with updated parameter matrix
#'
#' @details
#' Does not perform input checks!
#'
x2prms_vals = function(x, flex_prms_obj) {

  stopifnot(inherits(flex_prms_obj, "flex_prms"))

  # extract for easier adressing
  linear_internal_list = flex_prms_obj$linear_internal_list
  prms_matrix = flex_prms_obj$prms_matrix

  # iterate through all and update values according to x
  for (one_prm in colnames(prms_matrix)) {
    for (one_cond in rownames(prms_matrix)) {
      prm_pos = linear_internal_list[[one_prm]][[one_cond]]
      if (is.expression(prm_pos)) next
      if (prm_pos == 0) next
      prms_matrix[one_cond, one_prm] = x[prm_pos]
    }
  }

  # update the remaining special dependencies or custom parameters
  flex_prms_obj$prms_matrix = prms_matrix
  flex_prms_obj = update_special_values(flex_prms_obj)

  # and pass back
  return(flex_prms_obj)
}


# FUNCTIONS FOR MODIFYING/CARRYING OUT THE INSTR -----------------------------

#' Set Instructions to a flex_prms object
#'
#' Functions to carry out the "instructions" on how to modify a flex_prms
#' object, specified as a string.
#'
#' @param object an object of type `drift_dm` or `flex_prms`.
#'
#' @param instr a character string, specifying a set of instructions (see
#' Details).
#' @param ... further arguments passed forward to the respective method.
#'
#' @param eval_model logical, indicating if the model should be re-evaluated or
#'  not when updating modifying the flex_prms object
#'  (see [dRiftDM::re_evaluate_model]). Default is `FALSE`.
#' @param messaging logical, indicating if messages shall be ushered or not.
#' Can happen, for example, when setting specific parameter values in a
#' condition for which special dependencies were defined.
#'
#' @details
#' `modify_flex_prms` is a generic function. The default methods pass forward
#' a set of "instructions" to modify the (underlying) [dRiftDM::flex_prms]
#' object.
#'
#' These instructions are inspired by the model syntax of
#' the `lavaan` package. Note that
#' specifying multiple instructions is possible, but each instruction
#' has to be defined in its own line. Comments with '#' are possible, also
#' line continuations are possible, if the last symbol
#' is a "+","-", "*", "/", "(", or "[".
#' The following instructions are implemented:
#'
#' The **"vary"** instruction:
#'
#'  * Looks something like "a ~ foo + bar"
#'  * This means that the parameter 'a' is allowed to vary independently for the
#'  conditions 'foo' and 'bar'
#'  * Thus, when estimating the model, the user will have independent values
#'  for 'a' in conditions 'foo' and 'bar'
#'
#' The **"restrain"** instruction:
#'
#'  * Looks something like "a ~! foo + bar "
#'  * This means that the parameter 'a' is assumed to be identical for the
#'  conditions 'foo' and 'bar'
#'  * Thus, when estimating the model, the user will have only a single value
#'  for 'a' in conditions 'foo' and 'bar'
#'
#' The **"set"** instruction:
#'
#'  * Users may not always estimate a model directly but rather explore the
#'  model behavior. In this case setting the value of a parameter is necessary.
#'  * The corresponding instruction looks something like "a ~ foo => 0.3"
#'  * This will set the value for 'a' in condition 'foo' to the value of 0.3
#'
#' The **"fix"** instruction:
#'
#'  * Oftentimes, certain parameters of a model are considered "fixed", so that
#'  they don't vary while the remaining parameters are estimated. An example
#'  would be the shape parameter 'a' of DMC (see [dRiftDM::dmc_dm]).
#'  * The corresponding instruction looks something like "a <!> foo + bar"
#'  * Usually, users want to call the "set" instruction prior or after the "fix"
#'  instruction, to set the corresponding parameter to a certain value.
#'
#' The **"special dependency"** instruction:
#'
#'  * Sometimes, users wish to allow one parameter to depend on another. For
#'  instance, in DMC (see [dRiftDM::dmc_dm]), the parameter A is positive in
#'  the congruent condition, but negative in the incongruent condition. Thus,
#'  parameters may have a 'special depencency' which can be expressed as an
#'  equation.
#'  * To define a special dependency, users can use the operation "==".
#'  The parameter that should have the dependency is on the left-hand side,
#'  while the mathematical relationship to other parameters is defined on the
#'  right-hand side.
#'  * This then looks something like "a ~ foo == -(a ~ bar)".
#'  * This means that the parameter a in condition foo will always be
#'   -1 * the parameter a in condition bar. Thus, if a in condition bar
#'   has the value 5, then a in condition foo will be -5.
#'  * The expression on the right-side can refer to any arbitrary
#'  mathematical relation.
#'  * Important: Make sure that each 'parameter ~ condition' combination are set
#'  in brackets.
#'  * Another example: Parameter a in condition foo should be the mean of the
#'  parameter b in conditions bar and baz; this would be the instruction
#'  "a ~ foo == 0.5*(b ~ bar) + 0.5*(b ~ baz)"
#'
#'  The **"additional/custom parameter combination"** instruction:
#'
#'  * Sometimes, users may wish to combine multiple parameters to summarize
#'  a certain property of the model. For example, in DMC (see [dRiftDM::dmc_dm]),
#'  the shape and rate parameter jointly determine the peak latency.
#'  * To avoid to manually calculate this, users can define "custom"
#'  parameter combinations with the ":=" operation:
#'  * An examplary instruction might look like this:
#'    "peak_l := (a - 2) * tau"
#'  * Expressions and values that provide calculations for those parameters are
#'    stored in a separate list `cust_prms`.
#'
#'
#' @returns
#' For [dRiftDM::drift_dm] objects, the updated [dRiftDM::drift_dm] object.
#'
#' For [dRiftDM::flex_prms], the updated [dRiftDM::flex_prms] object.
#'
#' @seealso [flex_prms()]
#'
#' @export
modify_flex_prms = function(object, instr, ...){
  UseMethod("modify_flex_prms")
}


#' @rdname modify_flex_prms
#' @export
modify_flex_prms.drift_dm = function(object, instr, ..., eval_model = F) {

  # replace old flex_prms_object
  object$flex_prms_obj = modify_flex_prms(
    object$flex_prms_obj,
    instr = instr, ...
  )

  # ensure that everything is up-to-date
  object <- re_evaluate_model(
    drift_dm_obj = object,
    eval_model = eval_model
  )

  # ensure that nothing went wrong
  object <- validate_drift_dm(object)

  return(object)
}


#' @rdname modify_flex_prms
#' @export
modify_flex_prms.flex_prms = function(object, instr, ..., messaging = NULL) {

  flex_prms_obj = object

  if (is.null(messaging)) messaging = T

  if (!messaging) {
    flex_prms_obj = suppressMessages(
      modify_flex_prms(object = flex_prms_obj,
                       instr = instr) # messaging = F would lead to inf. recusion
    )
    return(flex_prms_obj)
  }

  # input checks
  if (!inherits(flex_prms_obj, "flex_prms"))
    stop("flex_prms_obj is not of type flex_prms")

  if (is.null(instr)) return(flex_prms_obj)

  if (!is.character(instr))
    stop("argument 'instr' must be character")

  instr = paste(instr, collapse = "\n")

  if (!is.logical(messaging) | length(messaging) != 1)
    stop("messaging must be single logical")

  # remove comments from instr
  instr <- gsub("#[^\\n]*", "", instr)

  # Remove line breaks if preceded by special characters or brackets
  # (We consider +, -, *, /, (, and [ as "special characters" here)
  instr <- gsub("([+*/\\(\\[\\-])\\s*\\n\\s*", "\\1 ", instr)

  sep_instr = unlist(strsplit(x = instr, split = "\n"))
  sep_instr = trimws(sep_instr)
  sep_instr = sep_instr[!sapply(sep_instr, is_empty)]

  # if there are multiple instructions, call each sequentially and pass back
  if (length(sep_instr) > 1) {
    for (one_instr in sep_instr) {
      flex_prms_obj = modify_flex_prms(object = flex_prms_obj,
                                       instr = one_instr)
    }
    return(flex_prms_obj)
  }

  # otherwise run the instruction
  one_instr = sep_instr

  vary_string_check = "^[\\w\\s\\+]*\\s*~\\s*[\\w\\s\\+]*\\s*$"
  if (grepl(vary_string_check, one_instr, perl = TRUE)) {
    # let parameters vary - instruction
    flex_prms_obj = flex_vary_prms(flex_prms_obj = flex_prms_obj,
                                   formula_instr = one_instr)

  } else if (grepl("~!", one_instr)) {
    # set parameter fixed - instruction
    flex_prms_obj = flex_restrain_prms(flex_prms_obj = flex_prms_obj,
                                       formula_instr = one_instr)

  } else if (grepl("=>", one_instr)) {
    # set a single parameter to a specific value
    flex_prms_obj = flex_specific_value(flex_prms_obj = flex_prms_obj,
                                        formula_instr = one_instr)

  } else if (grepl("<!>", one_instr)) {
    # exclude a parameter from estimation
    flex_prms_obj = flex_fix_prms(flex_prms_obj = flex_prms_obj,
                                  formula_instr = one_instr)

  } else if (grepl("==", one_instr)) {
    # set a special dependency
    flex_prms_obj = flex_special_dependency(flex_prms_obj = flex_prms_obj,
                                            formula_instr = one_instr)

  } else if (grepl(":=", one_instr)) {
    # set a custom parameters
    flex_prms_obj = flex_cust_prm(flex_prms_obj = flex_prms_obj,
                                  formula_instr = one_instr)
  } else {
    stop("couldn't interprete instruction: ", one_instr)
  }

  flex_prms_obj = validate_flex_prms(flex_prms_obj)

  return(flex_prms_obj)
}



# FUNCTIONS FOR CARRYING OUT THE INSTRUCTIONS -----------------------------

#' Allow parameters to vary
#'
#' This function takes an object of type flex_prms and a instruction string
#' that refers to a "vary" instruction (i.e., ab ~ cd + ds). This string
#' is broken down and unique parameters are introduced for the condition x
#' parameter combinations
#'
#' @param flex_prms_obj an object of type flex_prms
#' @param formula_instr a string referring to "vary"
#'  (see [dRiftDM::modify_flex_prms])
#'
#' @return an updated flex_prms_obj with an updated (linear) internal list
#'
flex_vary_prms = function(flex_prms_obj, formula_instr) {

  new_list = flex_prms_obj$internal_list

  prms_conds = prms_conds_to_modify(
    formula_instr = formula_instr, operation = "vary",
    all_conds = rownames(flex_prms_obj$prms_matrix),
    all_prms = colnames(flex_prms_obj$prms_matrix)
  )
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  # find the special links between parameters and maybe usher a warning
  for (one_prm in prms_to_adress) {
    one_entry = new_list[[one_prm]]
    for (one_cond in conds_to_adress) {
      cur_val = one_entry[[one_cond]]

      # check if the current value is an expression
      if (is.expression(cur_val)) {
        warning("Freeing parameter ", one_prm, " for condition ",
                one_cond, ". Special depencies were overwritten.")
      }
    }
  }

  # now iterate through all parameters and modify the internal list
  for (one_prm in prms_to_adress) {
    max_curr_number = max_number_one_internal_entry(new_list[[one_prm]])
    new_vals = (1:length(conds_to_adress)) + max_curr_number
    new_list[[one_prm]][conds_to_adress] = as.integer(new_vals)
    new_list[[one_prm]] = sort_one_internal_entry(new_list[[one_prm]])
  }

  # set and  re-linearize to ensure everything is up-to-date
  flex_prms_obj$internal_list = new_list
  new_linear_list = linearize_internal_list(internal_list = new_list)
  flex_prms_obj$linear_internal_list = new_linear_list

  return(flex_prms_obj)
}


#' Set parmaeters as equal across conditions
#'
#' This function takes a flex_prms object and modifies the (linear) internal
#' list so that a parameter is set as equal across multiple conditions,
#' according to the instruction formula (i.e., ' prm ~! conda + condb)
#'
#' @param flex_prms_obj flex_prms object
#' @param formula_instr a string referring to "restrain"
#'  (see [dRiftDM::modify_flex_prms])
#' @return a modified flex_prms object
#'
flex_restrain_prms = function(flex_prms_obj, formula_instr) {

  new_list = flex_prms_obj$internal_list
  new_prms_matrix = flex_prms_obj$prms_matrix

  prms_conds = prms_conds_to_modify(
    formula_instr = formula_instr, operation = "restrain",
    all_conds = rownames(new_prms_matrix),
    all_prms = colnames(new_prms_matrix)
  )
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  if (length(conds_to_adress) == 1) {
    warning("restraining parameter(s) ", paste(prms_to_adress, collapse = ", "),
            " only for one condition (", conds_to_adress, ") makes rarely",
            " sense; double-check if your instructions led to the intended",
            " result")
  }

  # find the special links between parameters and maybe usher a warning
  for (one_prm in prms_to_adress) {
    for (one_cond in conds_to_adress) {
      cur_val = new_list[[one_prm]][[one_cond]]
      # check if the current value is an expression
      if (is.expression(cur_val)) {
        warning("Restraining parameter ", one_prm, " for condition ",
                one_cond, ". Special depencies were overwritten.")
      }
    }
  }


  # now iterate through all parameters and modify the internal list
  for (one_prm in prms_to_adress) {
    max_curr_number = max_number_one_internal_entry(new_list[[one_prm]])
    new_vals = max_curr_number + 1
    new_list[[one_prm]][conds_to_adress] = as.integer(new_vals)
    new_list[[one_prm]] = sort_one_internal_entry(new_list[[one_prm]])
  }

  # afterward update the prms_matrix (average prms that are now fixed)
  for (one_prm in prms_to_adress) {
    cur_vals = new_prms_matrix[conds_to_adress, one_prm]
    new_prms_matrix[conds_to_adress, one_prm] = mean(cur_vals)
  }

  # set and re-linearize and check to ensure everything is up-to-date
  flex_prms_obj$internal_list = new_list
  new_linear_list = linearize_internal_list(internal_list = new_list)
  flex_prms_obj$linear_internal_list = new_linear_list

  return(flex_prms_obj)
}



#' Set a specific value to the parameter matrix (internal docu)
#'
#' This function takes a flex_prms_obj and sets certain values to the parameter
#' matrix, based on the given instruction string (i.e., ' prm ~ conda => 0.3)
#'
#' @param flex_prms_obj flex_prms object
#' @param formula_instr a string referring to "set"
#'  (see [dRiftDM::modify_flex_prms])
#' @returns an updated flex_prms object with a modified prms_matrix object
#'
flex_specific_value = function(flex_prms_obj, formula_instr) {

  # extract the necessary information
  prms_matrix = flex_prms_obj$prms_matrix
  internal_list = flex_prms_obj$internal_list

  # get the conditions and parameters to specify
  prms_conds = prms_conds_to_modify(
    formula_instr = formula_instr,
    operation = "set",
    all_conds = rownames(prms_matrix),
    all_prms = colnames(prms_matrix)
  )

  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  # find the number to set and cast it to numeric
  # a check of only => being present was already done
  formula_instr_sep = strsplit(formula_instr, "=>")[[1]]
  vals_to_set = formula_instr_sep[2]
  vals_to_set = strsplit(vals_to_set, split = "\\+")[[1]]
  vals_to_set = trimws(vals_to_set)
  if (length(vals_to_set) > 1) {
    if (length(vals_to_set) != length(conds_to_adress))
      stop("values right of => don't match with the number of conditions",
           " left of =>")
  }

  # convert the character input to numeric
  vals_to_set = sapply(vals_to_set, function(one_val){
    tryCatch(as.numeric(one_val),
             warning = function(w){
               stop("Couldn't convert input right of => to ",
                    "numbers. This is the expression for which ",
                    "conversions crashed; ", formula_instr)
             })
  })

  # find the special links between parameters and maybe usher a message
  for (one_prm in prms_to_adress) {
    one_entry = internal_list[[one_prm]]
    all_vals = unlist(one_entry[sapply(one_entry, is.numeric)])
    for (one_cond in conds_to_adress) {
      cur_val = one_entry[[one_cond]]

      # check if the current value is an expression
      if (is.expression(cur_val)) {
        warning("Setting a specific value for parameter ", one_prm,
                " in condition ", one_cond, ", which has a special dependency",
                " on other parameters. This will not have an effect.")

      } else {

        # check if conds_to_adress match with the number of conditions for
        # which a specific parameter is restrained
        ident_conds = names(all_vals)[which(cur_val == all_vals)]
        if (cur_val == 0) next()
        if (length(ident_conds) >= 2 & !all(ident_conds %in% conds_to_adress))
          message("Setting a specific value for parameter ", one_prm,
                  " in condition ", one_cond, ". This parameter is assumed to",
                  " be identical across conditions ",
                  paste(ident_conds, collapse = ", "), ".",
                  " Not problematic per se, just be aware of this"
          )

      }
    }
  }

  # then set the specific values
  prms_matrix[conds_to_adress, prms_to_adress] = vals_to_set
  flex_prms_obj$prms_matrix = prms_matrix

  # update the special dependencies etc:
  flex_prms_obj = update_special_values(flex_prms_obj)

  return(flex_prms_obj)
}



#' Exclude parameters from being modified (i.e., fix it; internal docu)
#'
#' This function modifies the (linear) internal list and sets the desired
#' parameters (based on the instruction string) to 0. This indicates that
#' this parameter is not altered within the function x2prms_vals
#' (i.e., ' prm <!> conda')
#'
#' @param flex_prms_obj a flex_prms object
#' @param formula_instr a string referring to "fix"
#'  (see [dRiftDM::modify_flex_prms])
#'
#' @return a modified flex_prms_obj with respect to the linear internal list
#'
flex_fix_prms = function(flex_prms_obj, formula_instr) {


  new_list = flex_prms_obj$internal_list

  prms_conds = prms_conds_to_modify(
    formula_instr = formula_instr, operation = "fix",
    all_conds = rownames(flex_prms_obj$prms_matrix),
    all_prms = colnames(flex_prms_obj$prms_matrix)
  )
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress


  # find the special links between parameters and maybe usher a warning
  for (one_prm in prms_to_adress) {
    one_entry = new_list[[one_prm]]
    for (one_cond in conds_to_adress) {
      cur_val = one_entry[[one_cond]]

      # check if the current value is an expression
      if (is.expression(cur_val)) {
        warning("Setting parameter ", one_prm, " as fixed for condition ",
                one_cond, ". Special depencies were overwritten.")
      }
    }
  }


  # now iterate through all parameters and modify the internal list
  for (one_prm in prms_to_adress) {
    new_list[[one_prm]][conds_to_adress] = 0L
    new_list[[one_prm]] = sort_one_internal_entry(new_list[[one_prm]])
  }

  # set and re-linearize and check to ensure everything is up-to-date
  flex_prms_obj$internal_list = new_list
  new_linear_list = linearize_internal_list(internal_list = new_list)
  flex_prms_obj$linear_internal_list = new_linear_list

  return(flex_prms_obj)
}




#' Set special dependencies (internal docu)
#'
#' Sets special dependencies so that parameters depend on other parameters.
#' (i.e., 'prmX ~ conda == -(prmY ~ condb)')
#'
#' @param flex_prms_obj a flex_prms object
#' @param formula_instr a string referring to "special dependency"
#'  (see [dRiftDM::modify_flex_prms])
#'
#' @returns a modified flex_prms_object with a modified (linear) internal list
#'
flex_special_dependency = function(flex_prms_obj, formula_instr) {

  new_list = flex_prms_obj$internal_list
  model_conds = rownames(flex_prms_obj$prms_matrix)
  model_prms = colnames(flex_prms_obj$prms_matrix)

  # find the parameters for which special dependencies shall be set
  prms_conds = prms_conds_to_modify(
    formula_instr = formula_instr,
    operation = "dependency",
    all_conds = model_conds,
    all_prms = model_prms
  )
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress



  # usher a message if setting parameter with special dependency on another
  # parameter that has a special instruction
  # also check if prm~cond combo is as on the left hand side and if legit names

  depends_on = trimws(strsplit(formula_instr, "==")[[1]][2])

  # find the parameters and conditions on which the intended dependency builds
  prms_regex = gregexpr("(?<=\\()[\\s]*[\\w]+[\\s]*(?=~)", depends_on,
                        perl = TRUE)
  conds_regex = gregexpr("(?<=~)[\\s]*[\\w]+[\\s]*(?=\\))", depends_on,
                         perl = TRUE)
  prms_depend <- unlist(regmatches(depends_on, prms_regex))
  prms_depend <- trimws(prms_depend)
  conds_depend <- unlist(regmatches(depends_on, conds_regex))
  conds_depend <- trimws(conds_depend)

  # check for legit names
  if (!any(prms_depend %in% model_prms)) {
    stop("prms on the right hand side don't match with the model parameters. ",
         "Found in ", formula_instr)
  }
  if (!any(conds_depend %in% model_conds)) {
    stop("conds on the right hand side don't match with the model parameters. ",
         "Found in ", formula_instr)
  }

  # go through all prms_conds_depends_on combos and check with prms_to_adress
  for (i in seq_along(conds_depend)) {
    prms_conds_depends = paste(prms_depend, conds_depend, sep = ".")
    prms_conds_adress = paste(prms_to_adress, conds_to_adress, sep = ".")

    if (any(prms_conds_depends %in% prms_conds_adress)) {
      stop("prms~conds on the right side match with prms~conds on the left ",
           "side. Recursive dependencies are not allowed. Found in ",
           formula_instr)
    }
  }


  # now iterate through all parameters and modify the internal list
  expr <- gsub("(\\w+)\\s*~\\s*(\\w+)", "prms_matrix['\\2', '\\1']", depends_on)
  for (i in seq_along(prms_to_adress)) {
    one_prm = prms_to_adress[i]
    for (one_cond in conds_to_adress) {
      new_list[[one_prm]][[one_cond]] = parse(text = expr)
    }
    new_list[[one_prm]] = sort_one_internal_entry(new_list[[one_prm]])
  }


  # set and re-linearize and check to ensure everything is up-to-date
  flex_prms_obj$internal_list = new_list
  new_linear_list = linearize_internal_list(internal_list = new_list)
  flex_prms_obj$linear_internal_list = new_linear_list

  # message when special dependencies are already present to be careful with
  # recursive dependencies
  unique_mat = internal_list_to_matrix(flex_prms_obj$internal_list)
  if (sum(unique_mat == "d") > 1) {
    message("Multiple special dependencies found. dRiftDM is not yet smart ",
            "enough to figure out all potential relationships and simply ",
            "processes special dependencies sequentially when iterating over ",
            "the parameter matrix. Double-check that this is not an issue in ",
            "your case (see print(<model>))")
  }


  # update the special dependencies
  flex_prms_obj = update_special_values(flex_prms_obj)

  return(flex_prms_obj)
}



#' Specify custom parameters
#'
#' This function takes a flex_prms_obj and adds or builds the entry `cust_prms`
#' to allow for custom parameters. An examplary instruction is
#' "peak_l = (a-1)*tau"
#'
#' The entry `cust_prms` is a list with entries `expressions` and `values`.
#' Each of these is again a named list, that either contains the expression
#' with instructions on how to calculate the custom parameter (e.g., "peak_l")
#' or the respective values. Values are getting updated/calculated in
#' `update_special_values()`
#'
#' @param flex_prms_obj a flex_prms object
#' @param formula_instr a string referring to "custom parameter combination"
#'  (see [dRiftDM::modify_flex_prms])
#'
#' @details
#' The `cust_prms` exists of two entries "expressions" and values".
#' "expressions" contains a named list, with expressions referring to
#' `prms_matrix` (see [dRiftDM::flex_prms]) on how to calculate the custom
#' parameter (across all conditions). The "values" contain a named list,
#' with named numeric vectors (names are conditions, values the calculated
#' custom parameter values)
#'
#'
#' @returns a modified flex_prms object with respect to the `cust_prms` entry
#'
flex_cust_prm = function(flex_prms_obj, formula_instr) {

  # extract the prm name on the left
  reg_ex <- "^\\s*(\\b\\w+)\\s*:=\\s*"
  cust_prm_name <- regmatches(formula_instr,
                              regexec(reg_ex, formula_instr))[[1]][2]

  if (is.na(cust_prm_name)) {
    stop("left side of ", paste("'", formula_instr, "'", sep = ""), "is not ",
         "a valid name")
  }

  # derive an expression for updating the custom parameter
  # separate the right side
  split_expr <- strsplit(formula_instr, ":=")[[1]]
  math_exp <- trimws(split_expr[2])  # Right-hand side (e.g., "(a - 2) * tau")

  if (is.na(math_exp)) {
    stop("right side of ", paste("'", formula_instr, "'", sep = ""),
         " is empty")
  }

  # insert the prms_matrix addition
  pattern <- "\\b(?!\\d+(\\.\\d+)?\\b)(\\w+)\\b"
  string_exp <- gsub(pattern, "prms_matrix[, '\\2']", math_exp, perl = T)

  # check that the parameter names are legit
  prms_regex = gregexpr(pattern, math_exp, perl = TRUE)
  depends_on = unlist(regmatches(math_exp, prms_regex))
  if (!all(depends_on %in% colnames(flex_prms_obj$prms_matrix))) {
    stop("right side of ", paste("'", formula_instr, "'", sep = ""),
         " specifies parameters that are not part of the model")
  }


  # make an expression and pack it up
  cust_expr = list(parse(text = string_exp))
  names(cust_expr) = cust_prm_name

  # add NA for the value to ensure the named entry is there
  place_holder = list(NA)
  names(place_holder) = cust_prm_name

  # build up the list (if it is non_existing)
  cust_prms = flex_prms_obj$cust_prms
  if (is.null(cust_prms)) {
    # add the expression
    cust_prms$expressions = cust_expr
    cust_prms$values = place_holder

  } else {

    if (cust_prm_name %in% names(cust_prms$expressions)) {
      message("Custom parameter ", cust_prm_name, " already exists.",
           " Replacing old one")
    }

    cust_prms$expressions[[cust_prm_name]] = NULL
    cust_prms$values[[cust_prm_name]] = NULL


    cust_prms$expressions = c(cust_prms$expressions, cust_expr)
    cust_prms$values = c(cust_prms$values, place_holder)
  }


  # update the values
  flex_prms_obj$cust_prms = cust_prms
  flex_prms_obj = update_special_values(flex_prms_obj)

  return(flex_prms_obj)
}


# FUNCTIONS FOR HANDLING/SORTING/COUNTING THE INTERNAL_LIST ---------------

#' Relabel the internal list
#'
#' The entries of the internal list are either digits (0-x) or expressions.
#' To ensure a valid mapping of these values to an input vector (as done when an
#' optimizer provides input parameters), we have to linearize the list
#' (done whenever modifying the list, see the different flex_* functions )
#'
#' @param internal_list the internal list, with entries for each parameter x
#' condition combination
#'
#' @returns
#'
#' another list, but with remapped digits in increasing order, while leaving
#' expressions or digits of 0 untouched.
#'
#' @seealso [dRiftDM::flex_vary_prms()], [dRiftDM::flex_restrain_prms()],
#' [dRiftDM::flex_fix_prms()], [dRiftDM::flex_special_dependency()]
#'
#'
linearize_internal_list = function(internal_list) {

  # count how many free parameters are there for each parameter
  count_prms = sapply(internal_list, count_unique_prms_one_internal_entry)
  cumsum_prms = as.integer(cumsum(count_prms))

  linear_internal_list = internal_list

  # create the continously increasing values for the number of parameters
  # this is a list containing the new values (requires the numbers in
  # internal list to be digits from 1 onward for those parameters that are free)
  mapping = lapply(seq_along(count_prms), function(i) {
    if (count_prms[i] == 0) {
      return(0)
    }
    start <- if (i == 1L) 1L else cumsum_prms[i - 1L] + 1L
    end <- cumsum_prms[i]
    start:end
  })


  # now do the remapping. For this end, iterate through the list and choose
  # the corresponding new value for those entries that are digits > 1
  # for each parameter (index) do...
  for (i in seq_along(linear_internal_list)) {

    # iterate through all conditions in flattened sublist
    for (j in seq_along(linear_internal_list[[i]])) {

      one_val = linear_internal_list[[i]][[j]]
      # it the actual entry is a a digit larger than 0, remap it
      if (check_digit_larger_0(one_val)) {
        linear_internal_list[[i]][[j]] = mapping[[i]][as.integer(one_val)]
      }
    }
  }

  # pass back the modified list (as a copy of course)
  return(linear_internal_list)
}




#' Count the number of digits > 0
#'
#' This function takes one entry of the internal_list (i.e., all conditions for
#' one parameter) and counts how often there are unique digits > 0
#'
#' @param one_internal_entry such as `internal_list[["A"]]`
#'
#' @returns a number
#'
count_unique_prms_one_internal_entry = function(one_internal_entry) {

  values = one_internal_entry[sapply(one_internal_entry, \(x) !is.expression(x))]
  if (length(values) == 0) # happens if there are only expressions
    return(0)
  values = unique(unlist(values))
  checks = sapply(values, check_digit_larger_0)
  return(sum(checks))
}


#' Check if an object of length <= 1 is either empty or contains entry with ""
#'
#' @param x a single value or empty vector
#'
#' @return TRUE or FALSE otherwise
is_empty <- function(x) {

  stopifnot(length(x) <= 1)

  (length(x) == 0) | (all(x == ""))
}


#' Extract the conditions and parameters from an instruction string
#'
#'
#' This function takes an instruction as a string and then extracts the
#' conditions and parameters, depending on what "type" of operation it is.
#' The expected structure is listed in the details docu of flex_prms()
#'
#'
#' @param formula_instr an instruction string
#' @param operation what to expect in terms of the string's structure. Can be
#'  "vary", "restrain", "fix", "set" or "dependency"
#' @param all_conds all potential conditions (necessary for extending missing
#' condition specification)
#' @param all_prms all potential paramters (necessary for extending missing
#' condition specification)
#'
#' @returns a named list prms_to_adress and conds_to_adress as character
#' vectors.
#'
prms_conds_to_modify = function(formula_instr, operation,
                                all_conds, all_prms) {

  # input checks
  stopifnot(is.character(formula_instr) & length(formula_instr) == 1)
  operation = match.arg(
    arg = operation,
    choices =  c("vary", "set", "restrain", "fix", "dependency")
  )
  stopifnot(is.character(all_conds) & length(all_conds) >= 1)
  stopifnot(is.character(all_prms) & length(all_prms) >= 1)


  # checks about the type of instruction should have happend prior to the call
  if (operation == "vary")
    split_vector <- strsplit(formula_instr, "~")[[1]]

  if (operation == "set")
    split_vector <- strsplit(formula_instr, "=>")[[1]]

  if (operation == "restrain")
    split_vector <- strsplit(formula_instr, "~!")[[1]]

  if (operation == "fix")
    split_vector <- strsplit(formula_instr, "<!>")[[1]]

  if (operation == "dependency")
    split_vector <- strsplit(formula_instr, "==")[[1]]


  if (length(split_vector) > 2 & operation == "vary") {
    stop("Only a single '~' allowed when setting parameters to vary; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "set") {
    stop("Only a single '~' allowed when setting specific parameter values; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "restrain") {
    stop("Only a single '~!' allowed when restraining parameters; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "fix") {
    stop("Only a single '<!>' allowed when setting parameters as fixed; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "dependency") {
    stop("Only a single '==' allowed when setting special dependencies; ",
         "found in: ", formula_instr)
  }


  # left hand side of == or => must be broken down further
  if (operation == "set" | operation == "dependency")
    split_vector = strsplit(split_vector[1], split = "~")[[1]]

  # check if multiple "~" were found
  if (length(split_vector) > 2) {
    stop ("there were more than one '~' to indicate conditions and parameters")
  }


  # happens for formula_instr like "~" (i.e., when there is nothing at the
  # start of the string; in this case add "" and proceed
  if (length(split_vector) == 1) {
    split_vector = append(split_vector, "")
  }

  # get the conditions and parameters to set
  prms_to_adress = split_vector[1]
  conds_to_adress = split_vector[2]

  # split based on + signs in case multiple parameters/conditions were specified
  prms_to_adress = strsplit(prms_to_adress, "\\+")[[1]]
  conds_to_adress = strsplit(conds_to_adress, "\\+")[[1]]

  # trim remaining white space
  prms_to_adress <- trimws(prms_to_adress)
  conds_to_adress <- trimws(conds_to_adress)


  # adress all prms if corresponding vector was ""
  if (all(sapply(prms_to_adress, is_empty))) {
    prms_to_adress = all_prms
  }

  # adress all conds if corresponding vector was ""
  if (all(sapply(conds_to_adress, is_empty))) {
    conds_to_adress = all_conds
  }

  # final checks
  if (!all(conds_to_adress %in% all_conds)) {
    stop("some conditions in the instructions don't match with the conditions",
         " of the model. Found in: ", formula_instr)
  }
  if (!all(prms_to_adress %in% all_prms)) {
    stop("some parameters in the instructions don't match with the parameters",
         " of the model. Found in: ", formula_instr)
  }

  return(list(prms_to_adress = prms_to_adress,
              conds_to_adress = conds_to_adress))
}



#' Get the maximum number from an internal entry or flex_prms_obj
#' (internal docu)
#'
#' The entries of the internal list are either digits (0-x) or expressions.
#'
#' @param one_internal_entry one entry of multiple conditions
#' @param flex_prms_obj a list stored in a flex_prms object
#'
#' @return the largest digit in the entry or of the linear_list in
#' the supplied flex_prms_obj (0 if there are only
#' expressions). The largest number of the linear_list corresponds to the number
#' of model parameters.
#'
max_number_one_internal_entry = function(one_internal_entry) {

  vals = sapply(one_internal_entry, function(x){
    if (is.expression(x)) return(0)
    return(x)
  })
  return(max(vals))
}

#' @rdname max_number_one_internal_entry
get_number_prms = function(flex_prms_obj) {
  vals = sapply(flex_prms_obj$linear_internal_list,
                max_number_one_internal_entry)
  return(max(vals))
}


#' Sorts the numbers in ascending order
#'
#' .... within one entry (i.e., one prm
#' across all conditions, such as when calling `internal_list[["A"]]`); which
#' is a list. Used to ensure that parameter ordering remains logical
#'
#' The entries of the internal list are either digits (0-x) or expressions.
#'
#' @param one_internal_entry one entry of a (linearized) internal_list, must be
#' named
#'
#' @returns the newly sorted entry as a list
#'
sort_one_internal_entry = function(one_internal_entry) {

  stopifnot(!is.null(names(one_internal_entry)))
  new_entry = one_internal_entry

  # works also for entries with only expressions
  numbers = new_entry[sapply(new_entry, check_digit_larger_0)]
  numbers = unlist(numbers)
  new_numbers = match(numbers, unique(numbers))
  new_entry[names(numbers)] = new_numbers

  return(new_entry)
}


#' Checks if a variable/vector of length 1 is a number > 0 or 0/expression
#'
#' assumed that x is either a digit or an expression
#'
#' @param x a vector of length 1
#'
#' @return a single logical
#'
check_digit_larger_0 = function(x) {

  stopifnot(length(x) == 1)
  stopifnot(is.expression(x) || is_numeric(x))

  if (is.expression(x)) return(F)
  return(x > 0)
}


#' Update all prms
#'
#' This function takes a flex_prms object and updates the prms_matrix according
#' to the special instructions in `internal_list` and the custom parameters
#' `cust_prms`
#'
#' @param flex_prms_obj a flex_prms object
#'
#' @returns the modified flex_prms_obj (i.e,. with the updated prms_matrix and
#'  the updated cust_prms$values)
#'
update_special_values = function(flex_prms_obj) {

  prms_matrix = flex_prms_obj$prms_matrix

  prm_names = colnames(flex_prms_obj$prms_matrix)
  cond_names = rownames(flex_prms_obj$prms_matrix)

  linear_internal_list = flex_prms_obj$linear_internal_list


  # update the special dependencies
  # iterate again to update all special dependencies
  for (one_prm in prm_names) {
    for (one_cond in cond_names) {
      cur_val = linear_internal_list[[one_prm]][[one_cond]]
      if (!is.expression(cur_val)) next
      prms_matrix[one_cond, one_prm] = eval(cur_val) # requires prms_matrix
    }
  }


  # update the custom parameters (if they exist)
  cust_prms = flex_prms_obj$cust_prms
  if (!is.null(cust_prms)) {
    for (one_name in names(cust_prms$expressions)) {
      cust_prms$values[[one_name]] = eval(cust_prms$expressions[[one_name]])
    }
  }

  # re-assemble and pass back
  flex_prms_obj$prms_matrix = prms_matrix
  flex_prms_obj$cust_prms = cust_prms

  return(flex_prms_obj)
}



#' checks if a flex_prms_object is parameterized in a reasonable way
#'
#'
#' * checks internal_list
#' * checks liner_internal_list
#' * checks the parameter matrix
#'
#' @param flex_prms_obj and object of type flex_prms
#'
#' @returns the unmodified flex_prms_obj object; only stop/warning statements
#'
validate_flex_prms = function(flex_prms_obj) {

  if(!inherits(flex_prms_obj, "flex_prms")) {
    stop ("provided flex_prms_obj argument is not of type flex_prms")
  }

  internal_list = flex_prms_obj$internal_list
  linear_internal_list = flex_prms_obj$linear_internal_list
  prms_matrix = flex_prms_obj$prms_matrix
  cust_prms = flex_prms_obj$cust_prms

  prm_names = colnames(flex_prms_obj$prms_matrix)
  cond_names = rownames(flex_prms_obj$prms_matrix)


  # check the matrix and names
  if (!is.character(cond_names) | length(cond_names) == 0) {
    stop("condition names are not a character vector of length >= 1")
  }

  if (any(cond_names == "")) {
    stop("condition with no name found in prms_matrix of flex_prms_obj")
  }

  if (any(grepl('\\W', cond_names, perl = T))) {
    stop("some condition name contain illegal non-alphanumeric characters")
  }

  if (!is.character(prm_names) | length(prm_names) == 0) {
    stop("parameter names are not a character vector of length >= 1")
  }

  if (any(prm_names == "")) {
    stop("parameter with no name found in prms_matrix of flex_prms_obj")
  }

  if (any(grepl('\\W', prm_names, perl = T))) {
    stop("some parameter names contain illegal non-alphanumeric characters")

  }

  if (anyDuplicated(cond_names) > 0) {
    stop("there are duplicate conditions names")
  }

  if (anyDuplicated(prm_names) > 0) {
    stop("there are duplicate parameters names")
  }



  # involves check that prms_matrix is a numeric matrix with unique parameter
  # entries
  for (one_cond in cond_names) {
    x = prms_matrix[one_cond,]
    names(x) = colnames(prms_matrix) # might be dropped if only 1 column exists
    check_if_named_numeric_vector(x = x,
                                  labels = prm_names,
                                  length = ncol(prms_matrix),
                                  var_name = "one row of parameter matrix")
  }


  # check the internal list
  if (!is.list(internal_list) | !is.list(linear_internal_list)) {
    stop("internal_list or linear_internal_list is not of type list")
  }
  internal_list = check_internal_list(internal_list = internal_list,
                                      prm_names = prm_names,
                                      cond_names = cond_names)

  # check the linear internal list
  linear_internal_list = check_internal_list(
    internal_list = linear_internal_list,
    prm_names = prm_names,
    cond_names = cond_names
  )

  # check the custom parameters
  if (!is.null(cust_prms)) {
    if (length(cust_prms) != 2 ||
        any(names(cust_prms) != c("expressions", "values"))) {
      stop("cust_prms not named with 'expressions' and 'values'")
    }
    check = sapply(cust_prms$expressions, \(x) !is.expression(x))
    if (any(check)){
      stop("'expressions' in cust_prms do not contain expressions")
    }
    if (any(names(cust_prms$expressions) != names(cust_prms$values)))
      stop("names in 'expressions' and 'values' don't match")
    for (cust_prm_vals in cust_prms$values) {
      check_if_named_numeric_vector(x = cust_prm_vals,
                                    labels = cond_names,
                                    length = nrow(prms_matrix),
                                    var_name = "a custom parameter")
    }
  }

  return(flex_prms_obj)
}





#' checks if all entries of internal_list are an expression or integer.
#' Throws an error if not (internal docu)
#'
#' Checks also if there are entries for each parameter and condition
#'
#' @param internal_list a list, referring to a (linear) internal list of
#' a [dRiftDM::flex_prms] object.
#' @param prm_names the expected parameter of the flex_prms object
#' @param cond_names the expected conditions of the flex_prms object
#'
#' @return the internal list for convenience
#'
check_internal_list = function(internal_list, prm_names, cond_names) {


  # parameter names check
  if (!isTRUE(all.equal(names(internal_list), prm_names))) {
    stop("an internal_list in flex_prms_object can not be adressed via",
         " the names ", paste(prm_names, collapse = ", "))
  }

  # condition names check and data type of entry
  check = lapply(internal_list, function(one_entry){

    if (!isTRUE(all.equal(names(one_entry), cond_names))) {
      stop("an entry of an internal_list in flex_prms_object can not be ",
           "adressed via the names ", paste(cond_names, collapse = ", "))
    }

    lapply(one_entry, function(val){
      if (is.expression(val)) return(TRUE)
      if (is.integer(val) & !is.na(val)) return(TRUE)
      return(FALSE)
    })

  })

  check = unlist(check)

  if (any(!check))
    stop("either an internal or linearized internal list provided values",
         " that are not valid integers or expressions")

  return(internal_list)
}



# turn an internal list to a matrix
internal_list_to_matrix = function(internal_list) {

  prms = names(internal_list)
  conds = names(internal_list[[1]])

  values = mapply(function(cond, prm){

    value = internal_list[[prm]][[cond]]
    if (is.numeric(value))
      return(value)
    return("d")

  }, conds, rep(prms, each = length(conds)))
  matrix = matrix(values, nrow = length(conds))
  rownames(matrix) = conds
  colnames(matrix) = prms

  return(matrix)
}




# METHODS TO EXTRACT FLEX_PRMS --------------------------------------------


#' @rdname flex_prms
#' @export
flex_prms.flex_prms <- function(object, ...) {
  return(object)
}

#' @rdname flex_prms
#' @export
flex_prms.drift_dm <- function(object, ...) {
  return(object$flex_prms_obj)
}


