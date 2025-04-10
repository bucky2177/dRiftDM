test_that("flex_prms -> creation works as expected", {
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
    conds = c("foo", "bar", "uff", "lorem")
  )

  # check basic structure (the rest is done when testing modify)
  expect_equal(
    names(a_flex_prms_obj),
    c("internal_list", "linear_internal_list", "prms_matrix")
  )

  expect_true(
    is.list(a_flex_prms_obj)
  )

  expect_true(
    is.list(a_flex_prms_obj$internal_list) &
      is.list(a_flex_prms_obj$linear_internal_list)
  )

  expect_true(
    is.matrix(a_flex_prms_obj$prms_matrix) &
      all.equal(dim(a_flex_prms_obj$prms_matrix), c(4, 3))
  )


  # flex_prms methods
  expect_equal(flex_prms(a_flex_prms_obj), a_flex_prms_obj)
  a_model <- list(flex_prms_obj = a_flex_prms_obj)
  class(a_model) <- "drift_dm"
  expect_equal(flex_prms(a_model), a_flex_prms_obj)
})


test_that("flex_prms -> input checks", {
  expect_error(
    flex_prms(numeric(), conds = c("foo", "bar")),
    "has length 0"
  )


  expect_error(
    flex_prms(c(ao.e = 3), conds = c("foo", "bar")),
    "illegal non-alphanumeric characters"
  )

  expect_error(
    flex_prms(c(aoe = 3), conds = c()),
    "not a character vector"
  )

  expect_error(
    flex_prms(c(aoe = 3), conds = c("comp", "comp")),
    "must be unique"
  )

  expect_error(
    flex_prms(c(aoe = 3), conds = c("cond!!")),
    "illegal non-alphanumeric characters"
  )
})


test_that("x2prms_vals works as expected", {
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
    conds = c("foo", "bar", "uff", "lorem"),
    instr = "a ~ foo + bar
                              b ~ bar == (a ~ foo) + 3
                              sum := a + b + c"
  )

  a_flex_prms_obj <- x2prms_vals(c(5, 8, 9, 10, 11), a_flex_prms_obj)

  exp_matrix <- matrix(c(
    5, 8, 9, 9,
    10, 8, 10, 10,
    11, 11, 11, 11
  ), nrow = 4)
  colnames(exp_matrix) <- c("a", "b", "c")
  rownames(exp_matrix) <- c("foo", "bar", "uff", "lorem")

  expect_equal(exp_matrix, a_flex_prms_obj$prms_matrix)

  expect_true(all(a_flex_prms_obj$cust_prms$values$sum == c(26, 27, 30, 30)))
})


test_that("modify_flex_prms -> all instructions work as expected", {
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
    conds = c("foo", "bar", "uff")
  )

  # vary
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "b ~ bar
                                              c ~ "
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$a),
    c(foo = 1, bar = 1, uff = 1)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$b),
    c(foo = 2, bar = 3, uff = 2)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$c),
    c(foo = 4, bar = 5, uff = 6)
  )

  # restrain
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "b ~! bar + uff
                                              c ~!"
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$a),
    c(foo = 1, bar = 1, uff = 1)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$b),
    c(foo = 2, bar = 3, uff = 3)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$c),
    c(foo = 4, bar = 4, uff = 4)
  )

  # set
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "a + c ~ => 0.3
                                              b ~ bar + uff => 0.4"
  )
  expect_equal(
    a_flex_prms_obj$prms_matrix[, 1],
    c(foo = 0.3, bar = 0.3, uff = 0.3)
  )
  expect_equal(
    a_flex_prms_obj$prms_matrix[, 2],
    c(foo = 3, bar = 0.4, uff = 0.4)
  )
  expect_equal(
    a_flex_prms_obj$prms_matrix[, 3],
    c(foo = 0.3, bar = 0.3, uff = 0.3)
  )

  # fix
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "a + b <!> foo
                                              c <!>"
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$a),
    c(foo = 0, bar = 1, uff = 1)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$b),
    c(foo = 0, bar = 2, uff = 2)
  )
  expect_equal(
    unlist(a_flex_prms_obj$linear_internal_list$c),
    c(foo = 0, bar = 0, uff = 0)
  )


  # special dependency
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "b + a ~ foo + bar == -(c ~ bar) * 2 / 3", messaging = F
  )
  expect_true(is.expression(a_flex_prms_obj$linear_internal_list$a$foo))
  expect_true(is.expression(a_flex_prms_obj$linear_internal_list$a$bar))
  expect_true(is_numeric(a_flex_prms_obj$linear_internal_list$a$uff))

  expect_true(is.expression(a_flex_prms_obj$linear_internal_list$b$foo))
  expect_true(is.expression(a_flex_prms_obj$linear_internal_list$b$bar))
  expect_true(is_numeric(a_flex_prms_obj$linear_internal_list$b$uff))

  expect_snapshot(a_flex_prms_obj)


  # custom parameter
  a_flex_prms_obj <- modify_flex_prms(
    object = a_flex_prms_obj,
    instr = "d := a + b
             e := a * c + b
             c ~ foo => 0.6"
  )
  expect_equal(
    a_flex_prms_obj$cust_prms$values$d,
    c(foo = -0.4, bar = -0.4, uff = 0.7)
  )
  expect_equal(
    a_flex_prms_obj$cust_prms$values$e,
    c(foo = -0.32, bar = -0.26, uff = 0.49)
  )

  # address all parameters
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
                               conds = c("foo", "bar"),
                               instr = "<!> bar"
  )

  expect_equal(a_flex_prms_obj$linear_internal_list$a,
               list(foo = 1, bar = 0))

  expect_equal(a_flex_prms_obj$linear_internal_list$b,
               list(foo = 2, bar = 0))
  expect_equal(a_flex_prms_obj$linear_internal_list$c,
               list(foo = 3, bar = 0))

})


test_that("validate_flex_prms -> errs as expected", {
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
    conds = c("foo", "bar", "uff"),
    instr = "a ~ foo == -(b ~ uff)
                                       d := c + b"
  )

  # internal_list checks
  temp <- a_flex_prms_obj
  temp$prms_matrix <- rbind(temp$prms_matrix, lorem = c(2, 3, 4))
  expect_error(
    validate_flex_prms(temp),
    "can not be adressed"
  )

  temp <- a_flex_prms_obj
  temp$prms_matrix <- cbind(temp$prms_matrix, lorem = c(2, 3, 4))
  expect_error(
    validate_flex_prms(temp),
    "can not be adressed"
  )

  temp <- a_flex_prms_obj
  temp$internal_list$a$bar <- NA
  expect_error(
    validate_flex_prms(temp),
    "not valid integers"
  )

  temp <- a_flex_prms_obj
  temp$linear_internal_list <- as.character(temp$linear_internal_list)
  expect_error(
    validate_flex_prms(temp),
    "not of type list"
  )

  # cond name checks
  temp <- a_flex_prms_obj
  rownames(temp$prms_matrix) <- NULL
  expect_error(
    validate_flex_prms(temp),
    "condition names are not a character vector"
  )

  temp <- a_flex_prms_obj
  rownames(temp$prms_matrix)[1] <- ""
  expect_error(
    validate_flex_prms(temp),
    "no name"
  )


  temp <- a_flex_prms_obj
  temp$prms_matrix <- matrix(NA, nrow = 3, ncol = 0)
  expect_error(
    validate_flex_prms(temp),
    "length >= 1"
  )

  temp <- a_flex_prms_obj
  rownames(temp$prms_matrix)[1] <- "a.e"
  expect_error(
    validate_flex_prms(temp),
    "illegal non-alphanumeric characters"
  )

  temp <- a_flex_prms_obj
  rownames(temp$prms_matrix)[c(1, 2)] <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "duplicate conditions"
  )



  # parameter name checks
  temp <- a_flex_prms_obj
  colnames(temp$prms_matrix) <- NULL
  expect_error(
    validate_flex_prms(temp),
    "parameter names are not a character vector"
  )

  temp <- a_flex_prms_obj
  colnames(temp$prms_matrix)[1] <- ""
  expect_error(
    validate_flex_prms(temp),
    "no name"
  )


  temp <- a_flex_prms_obj
  temp$prms_matrix <- matrix(NA, nrow = 0, ncol = 3)
  expect_error(
    validate_flex_prms(temp),
    "length >= 1"
  )

  temp <- a_flex_prms_obj
  colnames(temp$prms_matrix)[1] <- "a.e"
  expect_error(
    validate_flex_prms(temp),
    "illegal non-alphanumeric characters"
  )

  temp <- a_flex_prms_obj
  colnames(temp$prms_matrix)[c(1, 2)] <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "duplicate parameters"
  )

  # matrix type
  temp <- a_flex_prms_obj
  temp$prms_matrix[1, 1] <- as.character(temp$prms_matrix[1, 1])
  expect_error(validate_flex_prms(temp), "numeric")


  temp <- a_flex_prms_obj
  temp$prms_matrix[1, 1] <- NA
  expect_error(validate_flex_prms(temp), "NAs")


  # custom parameter checks
  temp <- a_flex_prms_obj
  temp$cust_prms$foo <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "not named with 'expressions' and 'values'"
  )

  temp <- a_flex_prms_obj
  names(temp$cust_prms)[1] <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "not named with 'expressions' and 'values'"
  )


  temp <- a_flex_prms_obj
  temp$cust_prms$expressions <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "do not contain expressions"
  )

  temp <- a_flex_prms_obj
  names(temp$cust_prms$values)[1] <- "foo"
  expect_error(
    validate_flex_prms(temp),
    "don't match"
  )

  temp <- a_flex_prms_obj
  temp$cust_prms$values$d <- as.character(temp$cust_prms$values$d)
  expect_error(
    validate_flex_prms(temp),
    "numeric"
  )
})


test_that("messages and warnings", {
  a_flex_prms_obj <- flex_prms(c(a = 2, b = 3, c = 5),
    conds = c("foo", "bar", "uff")
  )

  expect_warning(
    modify_flex_prms(a_flex_prms_obj, "a ~! foo"),
    "only for one condition"
  )



  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~
                     a ~ foo + bar => 0.3 + 0.4 + 0.5"),
    "don't match"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~ foo =>=> 0.4"),
    "Only a single"
  )


  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~
                     a ~ foo + bar => 0..5"),
    "Couldn't convert input"
  )

  expect_message(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar => 0.5"),
    "assumed to be identical"
  )


  expect_no_message(modify_flex_prms(a_flex_prms_obj,
    "a ~ bar => 0.5",
    messaging = F
  ))

  expect_warning(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ foo)
                                       a ~ bar => 0.3"),
    "which has a special dependency"
  )

  expect_warning(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ foo)
                                       a ~! bar + foo"),
    "overwritten"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~!~! bar + foo"),
    "Only a single"
  )

  expect_warning(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ foo)
                                       a ~"),
    "overwritten"
  )


  expect_warning(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ foo)
                                       a ~"),
    "overwritten"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(muc ~ foo)"),
    "prms on the right hand side"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ fo)"),
    "conds on the right hand side"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~ bar == -(a ~ bar)"),
    "Recursive"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "a ~~ bar == -(a ~ bar)"),
    "there were more than one '~"
  )


  expect_error(
    modify_flex_prms(a_flex_prms_obj, "e?e := a"),
    "not a valid name"
  )


  expect_error(
    modify_flex_prms(a_flex_prms_obj, "e := "),
    "empty"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, "e := a + d "),
    "parameters that are not part of the model"
  )

  expect_message(
    modify_flex_prms(a_flex_prms_obj, "e := a + c
                                       e := a + b"),
    "parameter e already exists. Replacing old one"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, instr = 5),
    "'instr' must be character"
  )
  expect_error(
    modify_flex_prms(a_flex_prms_obj, instr = "a ~ ", messaging = "foo"),
    "messaging must be a single logical"
  )
  expect_error(
    modify_flex_prms(a_flex_prms_obj, instr = "a !! "),
    "couldn't interprete instruction: a !!"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, instr = "a <!> foo <!> to"),
    "Only a single"
  )

  expect_error(
    modify_flex_prms(a_flex_prms_obj, instr = "a ~ foo == == b ~ bar"),
    "Only a single"
  )
})


test_that("modify_flex_prms -> drift_dm", {
  a_model <- readRDS(test_path("fixtures", "dmc.rds"))

  a_model <- modify_flex_prms(a_model, instr = "muc ~
                             alpha ~ => 5")

  expect_equal(
    a_model$flex_prms_obj$prms_matrix[, "alpha"],
    c(comp = 5, incomp = 5)
  )
})
