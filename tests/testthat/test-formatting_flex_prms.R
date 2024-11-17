test_that("summary.flex_prms and corresponding print works as expected", {

  a_flex_prms = flex_prms(c(a = 2, b = 3, c = 4), conds = c("foo", "bar", "ho"),
                          instr = "a ~
                          b ~ bar == ((a ~ foo) + -(c ~ bar))
                          b ~ ho == -(a ~ foo)
                          c <!> foo
                          lorem := (a + 2) * b / c
                          c ~ bar => 10", messaging = F)

  sum_obj = summary(a_flex_prms)

  # check properties
  expect_identical(
    names(sum_obj),
    c("prms_matrix", "unique_matrix", "depend_strings", "cust_prms_matrix")
  )

  # check matrix
  expect_identical(
    sum_obj$prms_matrix,
    a_flex_prms$prms_matrix
  )

  # check unique matrix
  exp_matrix = matrix(c(1,2,3,4, "d", "d", 0, 5,5), nrow = 3, ncol = 3)
  rownames(exp_matrix) = c("foo", "bar", "ho")
  colnames(exp_matrix) = c("a", "b", "c")
  exp_matrix["bar", "b"] = "d"
  expect_identical(
    sum_obj$unique_matrix,
    exp_matrix
  )

  # check strings
  expect_identical(
    sum_obj$depend_strings,
    c("b ~ bar == ((a ~ foo) + -(c ~ bar))", "b ~ ho == -(a ~ foo)")
  )

  # check cust_prms
  exp_cust =
  expect_identical(
    as.numeric(sum_obj$cust_prms_matrix),
    c(3, -3.2, -2)
  )


  ## here prints
  expect_snapshot(
    print(sum_obj)
  )

  expect_snapshot(
    print(a_flex_prms)
  )

})
