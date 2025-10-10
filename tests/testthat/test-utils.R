test_that("global variables are as expected", {
  expect_identical(drift_dm_approx_error(), 1e-20)
  expect_identical(drift_dm_medium_approx_error(), 1e-06)
  expect_identical(drift_dm_small_approx_error(), .01)
  expect_identical(drift_dm_rough_approx_error(), .1)
  expect_identical(drift_dm_robust_prm(), 1e-8)
  expect_identical(drift_dm_default_rounding(), 3)
  expect_identical(drift_dm_default_probs(), seq(0.1, 0.9, 0.1))

  b_coding_list <- drift_dm_default_b_coding()
  expect_identical(b_coding_list$column, "Error")
  expect_identical(b_coding_list$u_name_value, c(corr = 0))
  expect_identical(b_coding_list$l_name_value, c(err = 1))
  expect_identical(length(b_coding_list), 3L)
})


test_that("prms_to_str works as expected", {
  expect_equal(
    prms_to_str(c("as", "bas", "mu_"), c(1, 2, 3)),
    "as=>1\nbas=>2\nmu_=>3"
  )

  expect_identical(
    prms_to_str(
      x = c("a", "b", "d"), prms = c(1, 2, 3),
      round_digits = 3, collapse = c(";", "!"),
      sep = c("!", "#")
    ),
    "a!1;b!2;d!3"
  )

  expect_identical(
    prms_to_str(dmc_dummy),
    "muc=>4\nb=>0.6\nnon_dec=>0.3\nsd_non_dec=>0.02\ntau=>0.04\nA=>0.1\nalpha=>4"
  )
})


test_that("prms_to_str input checks", {
  expect_error(prms_to_str(
    x = c("as", "bas", "mu_"), prms = rnorm(3),
    round_digits = NA
  ), "not a valid numeric")
  expect_error(prms_to_str(
    x = c(1, 2, 3), prms = rnorm(3),
    round_digits = 3
  ), "not of type character")
  expect_error(prms_to_str(
    x = c("a", "b", "c"), prms = c("1", 2, 3),
    round_digits = 3
  ), "not a valid numeric")

  expect_error(prms_to_str(
    x = c("a", "b"), prms = c(1, 2, 3),
    round_digits = 3
  ), "don't match")

  expect_error(prms_to_str(
    x = character(), prms = numeric(),
    round_digits = 3
  ), "are of length zero")

  expect_error(
    prms_to_str(
      x = c("a", "b", "d"), prms = c(1, 2, 3),
      round_digits = 3, collapse = NA
    ),
    "not of type character"
  )
})


test_that("check_if_named_vector input checks", {
  expect_error(
    check_if_named_numeric_vector(c("1", 2, 3), "x"),
    "numeric vector"
  )
  expect_error(
    check_if_named_numeric_vector(c(1, 2, 3), "x", length = 2),
    "2 entries"
  )
  expect_error(
    check_if_named_numeric_vector(c(1, 2, 3), "x", length = 3),
    "ensure that x is a named vector"
  )
  expect_error(
    check_if_named_numeric_vector(c(a = 1, b = 2, c = 3), "x",
      labels = c("a", "x", "z")
    ),
    "can not be adressed"
  )

  expect_error(
    check_if_named_numeric_vector(c(a = NA, b = 2, c = 3), "x"),
    "NAs"
  )

  expect_error(
    check_if_named_numeric_vector(numeric(), "x"),
    "empty vector"
  )

  expect_error(
    check_if_named_numeric_vector(c(a = 2, 3), "x"),
    "for each entry"
  )

  expect_error(
    check_if_named_numeric_vector(c(a = 2, b = 3), "foo", c("x", "x")),
    "duplicate"
  )

  expect_error(
    check_if_named_numeric_vector(c(a = 2, a = 3), "foo", c("x", "y")),
    "duplicate"
  )

  expect_warning(
    check_if_named_numeric_vector(c(a = Inf, b = 3), "foo"),
    "infinite"
  )
  expect_error(
    check_if_named_numeric_vector(c(a.3 = 2, b = 3), "foo"),
    "characters"
  )
})


test_that("prm_cond_combo_2_labels and prms_cond_combo", {
  # test case 1
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 2), conds = c("i", "c"),
    subclass = "test"
  )

  prms_cond_combo_1 <- prms_cond_combo(a_model)
  expect_identical(
    prms_cond_combo_1,
    matrix(c("a", "b", "i", "i"), nrow = 2, byrow = T)
  )

  expect_identical(
    prm_cond_combo_2_labels(prms_cond_combo_1),
    c("a", "b")
  )

  # test case 2
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 2, c = 2),
    conds = c("i", "c", "d"),
    subclass = "test", instr = "b ~ "
  )

  prms_cond_combo_2 <- prms_cond_combo(a_model)
  expect_identical(
    prms_cond_combo_2,
    matrix(c(
      "a", "b", "b", "b", "c",
      "i", "i", "c", "d", "i"
    ), nrow = 2, byrow = T)
  )

  expect_identical(
    prm_cond_combo_2_labels(prms_cond_combo_2),
    c("a", "b.i", "b.c", "b.d", "c")
  )
})


test_that("prm_con_combo_2_labels input checks", {
  temp <- matrix(sample(1:10, 6, TRUE))
  expect_error(
    prm_cond_combo_2_labels(temp),
    "is.character"
  )

  temp <- matrix(sample(LETTERS, 6, TRUE))
  expect_error(
    prm_cond_combo_2_labels(temp),
    "nrow"
  )

  expect_error(
    prm_cond_combo_2_labels(as.vector(temp)),
    "is.matrix"
  )
})


test_that("get_parameters_smart works as expected", {
  # test case 1 - just vectors
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 2, c = 2),
    conds = c("i", "c"),
    subclass = "test", instr = "b ~ "
  )

  expect_list <- list(
    vec_a = c("a" = 1, "b.i" = 2, "b.c" = 2, "c" = 3),
    vec_b = c("a" = 4, "b.i" = 5, "b.c" = 5, "c" = 6)
  )
  expect_identical(
    get_parameters_smart(a_model, c(1, 2, 3), c(4, 5, 6)),
    expect_list
  )


  # continue with test case 2 - named numeric vectors
  expect_identical(
    get_parameters_smart(
      a_model, c(b = 2, a = 1, c = 3),
      c(a = 4, c = 6, b = 5)
    ),
    expect_list
  )

  # continue with test case 3 - lists
  expect_identical(
    get_parameters_smart(
      a_model,
      list(default_values = c(1, 2, 3)),
      list(default_values = c(c = 6, b = 5, a = 4))
    ),
    expect_list
  )


  # continue with test case 3 - lists, but with special variation
  expect_list$vec_a["b.i"] <- 4
  expect_identical(
    get_parameters_smart(
      a_model,
      list(
        default_values = c(a = 1, b = 2, c = 3),
        i = c(b = 4)
      ), # in cond i, let lower of b be 4
      list(default_values = c(c = 6, b = 5, a = 4))
    ),
    expect_list
  )

  # final check for label
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 2, c = 2),
    conds = c("i", "c"),
    subclass = "test", instr = "b ~ "
  )

  expect_list <- list(
    vec_a = c(1, 2, 2, 3),
    vec_b = c(4, 5, 5, 6)
  )
  expect_identical(
    get_parameters_smart(
      a_model,
      c(1, 2, 3),
      c(4, 5, 6),
      labels = F
    ),
    expect_list
  )
})


test_that("trapz works on a simple linear function", {
  x <- 0:4
  y <- 2 * x + 1  # integral from 0..4 of (2x+1) = [x^2 + x]_0^4 = 20
  expect_equal(trapz(x, y), 20)
})

test_that("cumtrapz returns cumulative integral starting at 0", {
  x <- 0:3
  y <- x  # integral of x dx over [0,1],[1,2],[2,3]: 0.5, 1.5, 2.5; cumul: 0,0.5,2.0,4.5
  res <- cumtrapz(x, y)
  expect_equal(res, c(0, 0.5, 2.0, 4.5))
  # last element should match trapz
  expect_equal(tail(res, 1), trapz(x, y))
})

test_that("trapz errors as expected", {
  expect_error(trapz(0:2, c(1, 2)), "same length")
  expect_error(trapz(0, 1), "at least two points")

  expect_error(trapz(0:2, c("a", "b", "c")), "valid numerics")

  expect_error(trapz(c(0, 1, 1, 2), c(0, 1, 2, 3)), "strictly increasing")
  expect_error(trapz(c(2, 1, 0), c(1, 2, 3)), "strictly increasing")
})

test_that("numerical accuracy vs analytic integral for a smooth function", {
  # f(x) = sin(x) on [0, pi]; integral = 2
  x <- seq(0, pi, length.out = 201)
  y <- sin(x)
  expect_equal(trapz(x, y), 2, tolerance = 1e-4)
})


test_that("get_parameters_smart input checks", {
  # general input errors
  a_model <- drift_dm(
    prms_model = c(a = 2, b = 2, c = 2),
    conds = c("i", "c"),
    subclass = "test", instr = "b ~ "
  )
  expect_error(
    get_parameters_smart(
      a_model,
      input_a = c("1", "2", "3"),
      input_b = c("1")
    ), "illegal data type"
  )

  expect_error(
    get_parameters_smart(
      a_model,
      input_a = c(1, 2),
      input_b = c("1")
    ), "doesn't match"
  )

  expect_error(
    get_parameters_smart(
      a_model,
      input_a = c(1, 2, 3),
      input_b = c(1, 2, 3),
      labels = NULL
    ), "is.logical"
  )

  # check if lower < upper
  expect_warning(
    get_parameters_smart(
      a_model,
      input_a = c(1, 2, 3),
      input_b = c(0, 2, 3)
    ), "larger than"
  )

  # check from list formation
  expect_error(
    get_parameters_smart(
      a_model,
      list(
        i = c(b = 4),
        i = c(b = 4)
      ), # in cond i, let lower of b be 4
      list(default_values = c(c = 6, b = 5, a = 4))
    ),
    "with the name \\'default_values\\'"
  )

  expect_error(
    get_parameters_smart(
      a_model,
      list(
        default_values = c(a = 1, b = 2, c = 3),
        i = c(a = 4)
      ),
      list(default_values = c(c = 6, b = 5, a = 4))
    ),
    "this parameter is identical across conditions"
  )
})


test_that("get_example_fits", {

  # some rough checks, as this is merely a helper function to get some
  # example fit objects

  # fits_ids
  aux_fits <- get_example_fits("fits_ids")
  expect_s3_class(aux_fits, "fits_ids_dm")

  expect_identical(
    aux_fits$drift_dm_fit_info$obs_data_ids,
    ulrich_flanker_data[ulrich_flanker_data$ID %in% 1:3, ]
  )

  expect_identical(
    class(aux_fits$drift_dm_fit_info$drift_dm_obj),
    c("dmc_dm", "drift_dm")
  )

  # fits_agg_dm
  aux_fits <- get_example_fits("fits_agg")
  expect_s3_class(aux_fits, "fits_agg_dm")
  expect_identical(class(aux_fits$drift_dm_obj), c("ratcliff_dm", "drift_dm"))


  # mcmc
  aux_fits <- get_example_fits("mcmc")
  expect_s3_class(aux_fits, "mcmc_dm")
  expect_identical(
    class(attr(aux_fits, "data_model")), c("ratcliff_dm", "drift_dm")
  )
})



