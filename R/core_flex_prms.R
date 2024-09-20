###########################
# methods of flexible parameter setting

flex_prms = function(prms_model, conds, instr=NULL) {

  # TODO: input checks (ensure no special symbols in prm names and conds)

  # prms_model => named numeric vector
  # conds => string vector
  prms_vals = unname(prms_model)
  name_prms_model = names(prms_model)


  # create the internal_list
  internal_list <- setNames(vector("list", length(name_prms_model)), name_prms_model)
  internal_list = lapply(internal_list, function(one_internal_entry){
    one_internal_entry = as.list(rep(1, length(conds)))
    one_internal_entry = setNames(one_internal_entry, conds)
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

  class(flex_prms_obj) <- "flex_prms_dm"


  # run optional instructions
  flex_prms_obj = modify_flex_prms(flex_prms_obj = flex_prms_obj,
                                   instr = instr)
}




linearize_internal_list = function(internal_list) {


  # TODO

  count_prms = sapply(internal_list, count_prms_one_internal_entry)
  cumsum_prms = as.integer(cumsum(count_prms))

  mapping = lapply(seq_along(count_prms), function(i) {
    start <- if (i == 1L) 1L else cumsum_prms[i-1L] + 1L
    end <- cumsum_prms[i]
    start:end
  })



  # iterate through all prms in the internal_list
  for (i in seq_along(internal_list)) {
    flattened = unlist(internal_list[[i]])

    # iterate through all entries in the internal_list
    for (j in seq_along(flattened)) {

      one_val = flattened[j]
      # it the actual entry is a a digit, remap it
      if (grepl("^\\d+$", one_val)) {
        internal_list[[i]][[j]] = mapping[[i]][as.integer(one_val)]
      }
    }
  }

  return(internal_list)
}




x2prms_vals = function(x, linear_internal_list, prms_vals_matrix) {




  update_matrix <- function(prm_name, cond_name) {

    one_lin_internal = linear_internal_list[[prm_name]][[cond_name]]

    if (is.integer(one_lin_internal)) {
      return(x[one_lin_internal])
    } else {
      return(one_lin_internal) # has to be parsed
    }
  }

  prm_names = colnames(prms_vals_matrix)
  prm_names_rep = rep(prm_names, each = nrow(prms_vals_matrix))
  cond_names = rownames(prms_vals_matrix)

  new_prms_vals_matrix = mapply(update_matrix, prm_name = prm_names_rep,
                                cond_name = cond_names)
  new_prms_vals_matrix = matrix(new_prms_vals_matrix,
                                nrow = length(cond_names),
                                byrow = F)

  colnames(new_prms_vals_matrix) = prm_names
  rownames(new_prms_vals_matrix) = cond_names

  return(new_prms_vals_matrix)
}




set_flex_prms = function(flex_prms_obj, instr, warn = T) {


  sep_instr = unlist(strsplit(x = instr, split = "\n"))

  if (length(sep_instr) > 1) {
    for (one_instr in sep_instr) {
      flex_prms_obj = set_flex_prms(flex_prms_obj = flex_prms_obj,
                                    instr = one_instr)
    }
    return(flex_prms_obj)
  }

  one_instr = sep_instr

  # let parameters vary - instruction
  if (grepl("~ ?(?!\\!)", one_instr, perl = TRUE) & !grepl("==", one_instr)) {
    flex_prms_obj = let_prms_vary(flex_prms_obj = flex_prms_obj,
                                  formula_instr = one_instr)
  }

  # set parameter fixed - instruction
  if (grepl("~!", one_instr)) {
    flex_prms_obj = set_prms_fixed(flex_prms_obj = flex_prms_obj,
                                   formula_instr = one_instr)
  }

  # set a single parameter to a specific value
  if (grepl("=>", one_instr)){
    flex_prms_obj = set_specific_value(flex_prms_obj = flex_prms_obj,
                                       formula_instr = one_instr, warn = warn)
  }


  # TODO: Set special depencies

  return(flex_prms_obj)
}


set_specific_value = function(flex_prms_obj, formula_instr, warn = T) {


  formula_instr_sep = strsplit(formula_instr, "=>")[[1]]

  if (length(formula_instr_sep) != 2) {
    stop("more than one => symbol found when setting specific values",
         "found in: ", formula_instr)
  }

  prms_conds = prms_conds_to_modify(formula_instr = formula_instr_sep[1],
                                    operation = "set")
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  # find the number to set
  vals_to_set = formula_instr_sep[2]
  vals_to_set = strsplit(vals_to_set, split = "\\+")[[1]]
  vals_to_set = trimws(vals_to_set)
  if (length(vals_to_set) > 1) {
    if (length(vals_to_set) != length(conds_to_adress))
      stop(" values right of => don't match with the number of conditions",
           " left of =>")
  }

  vals_to_set = sapply(vals_to_set, function(one_val){
    tryCatch(as.numeric(one_val),
             warning = function(w){
               stop("Couldn't convert input right of => to ",
                    "numbers. This is the expression for which ",
                    "conversions crashed; ", formula_instr)
             })
  })

  # find the special links between parameters and maybe usher a warning
  if (warn) {

    for (one_prm in prms_to_adress) {
      one_entry = flex_prms_obj$internal_list[[one_prm]]
      for (one_cond in conds_to_adress) {
        cur_val = one_entry[[one_cond]]

        # check if the current value is an expression
        if (is.expression(cur_val)) {
          warning("Setting a specific value for parameter ", one_prm,
                  " in condition ", one_cond, ", which has a special depency ",
                  "on other parameters. Not problematic per se, ",
                  "just be aware of this"
                  )
        }

        # check if conds_to_adress match with the number of conditions for
        # which a specific parameter is fixed
        all_vals = unlist(one_entry[sapply(one_entry, is.numeric)])
        ident_conds = names(all_vals)[which(cur_val == all_vals)]
        if (length(ident_conds) >= 2 & any(ident_conds %in% conds_to_adress)) {
          warning("Setting a specific value for parameter ", one_prm,
                  " in condition ", one_cond, ". This parameter is assumed to ",
                  " be identical across conditions ",
                  paste(ident_conds, collapse = ", "), ".",
                  " Not problematic per se, just be aware of this"
          )
        }
      }
    }

  }


  # then set the specific value
  prms_matrix[conds_to_adress, prms_to_adress] = val_to_set

}

let_prms_vary = function(flex_prms_obj, formula_instr) {

  internal_list = flex_prms_obj$internal_list

  prms_conds = prms_conds_to_modify(formula_instr = formula_instr,
                                    operation = "free")
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  # now iterate through all parameters and modify the internal list
  for (one_prm in prms_to_adress) {
    max_curr_number = max_number_one_internal_entry(internal_list[[one_prm]])
    new_vals = (1:length(conds_to_adress)) + max_curr_number
    internal_list[[one_prm]][conds_to_adress] = new_vals
    internal_list[[one_prm]] = sort_one_internal_entry(
      internal_list[[one_prm]]
    )
  }

  # re-linearize if necessary
  flex_prms_obj$linear_internal_list = linearize_internal_list(
    internal_list = internal_list
  )

  return(flex_prms_obj)
}


prms_conds_to_modify = function(formula_instr, operation = "set") {

  if (operation == "free" | operation == "set")
    split_vector <- strsplit(formula_instr, "~")[[1]]

  if (operation == "fix")
    split_vector <- strsplit(formula_instr, "~!")[[1]]


  if (length(split_vector) > 2 & operation == "free") {
    stop("Only a single '~' allowed when setting parameters to vary; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "set") {
    stop("Only a single '~' allowed when setting specific parameter values; ",
         "found in: ", formula_instr)
  }

  if (length(split_vector) > 2 & operation == "fix") {
    stop("Only a single '~!' allowed when setting parameters as fixed; ",
         "found in: ", formula_instr)
  }


  # happens for formula_instr == "~" or == "~!"
  # in this case add "" and proceed
  if (length(split_vector) == 1) {
    split_vector = append(split_vector, "")
  }

  conds_to_adress = split_vector[1]
  prms_to_adress = split_vector[2]

  conds_to_adress = strsplit(conds_to_adress, "\\+")[[1]]
  prms_to_adress = strsplit(prms_to_adress, "\\+")[[1]]

  conds_to_adress <- trimws(conds_to_adress)  # Removes leading/trailing whitespace
  prms_to_adress <- trimws(prms_to_adress)

  # adress all conds if corresponding vector was empty
  if (is_empty(conds_to_adress)) {
    conds_to_adress = rownames(flex_prms_obj$prms_matrix)
  }

  # adress all prms if corresponding vector was empty
  if (is_empty(prms_to_adress)) {
    prms_to_adress = colnames(flex_prms_obj$prms_matrix)
  }

  return(list(prms_to_adress = prms_to_adress,
              conds_to_adress = conds_to_adress))
}



set_prms_fixed = function(flex_prms_obj, formula_instr) {

  internal_list = flex_prms_obj$internal_list

  prms_conds = prms_conds_to_modify(formula_instr = formula_instr,
                                    operation = "fix")
  prms_to_adress = prms_conds$prms_to_adress
  conds_to_adress = prms_conds$conds_to_adress

  # now iterate through all parameters and modify the internal list
  for (one_prm in prms_to_adress) {
    max_curr_number = max_number_one_internal_entry(internal_list[[one_prm]])
    new_vals = max_curr_number + 1
    internal_list[[one_prm]][conds_to_adress] = new_vals
    internal_list[[one_prm]] = sort_one_internal_entry(
      internal_list[[one_prm]]
    )
  }

  # TODO
  flex_prms_obj$linear_internal_list = linearize_internal_list(
    internal_list = internal_list
  )

  return(flex_prms_obj)
}


is_empty <- function(x) {
  (length(x) == 0) || (length(x) == 1 && x == "")
}


count_prms_one_internal_entry = function(one_internal_entry) {

  flattened = unlist(one_internal_entry)
  flattened = unique(flattened)
  counted_digits = sum(grepl("^\\d+$", flattened))

  return(counted_digits)
}


max_number_one_internal_entry = function(one_internal_entry) {

  flattened = unlist(one_internal_entry)
  flattened = flattened[grepl("^\\d+$", flattened)]
  return(max(as.numeric(flattened)))
}


sort_one_internal_entry = function(one_internal_entry) {

  numbers = one_internal_entry[sapply(one_internal_entry, is.numeric)]
  new_numbers = match(unlist(numbers), unique(numbers))
  one_internal_entry[names(numbers)] = new_numbers

  return(one_internal_entry)
}


# TODO: CHECK flexprms
