test_that("print.traces_dm_list works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))
  traces <- simulate_traces(a_model, k = 2, seed = 1)

  expect_snapshot(
    print(traces)
  )
})


test_that("print.traces_dm works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))
  traces <- simulate_traces(a_model, k = 5, seed = 1)
  traces <- unpack_obj(traces, unpack_elements = F, conds = "comp")

  expect_snapshot(
    print(traces, print_steps = 6, print_k = 3)
  )
})



test_that("summary.traces_dm works as expected", {

  withr::local_preserve_seed()
  set.seed(1)

  some_model <- dmc_dm()
  traces_dm_obj <- simulate_traces(some_model, k = 10)$comp
  summary_traces <- summary(traces_dm_obj)

  # Check class
  expect_s3_class(summary_traces, "summary.traces_dm")

  # Check stored attributes
  expect_identical(summary_traces$k, 10L)
  expect_identical(summary_traces$add_x, FALSE)
  expect_identical(summary_traces$orig_model_class, c("dmc_dm", "drift_dm"))
  expect_identical(summary_traces$orig_prms,
                   some_model$flex_prms_obj$prms_matrix["comp",])
  expect_identical(summary_traces$prms_solve, some_model$prms_solve)

  # Check first passage time statistics
  idx_fpt <- apply(traces_dm_obj, 1, function(x) max(which(!is.na(x))))
  t_vec <- seq(0, some_model$prms_solve["t_max"],
               length.out = some_model$prms_solve["nt"] + 1)
  ts_fpt <- t_vec[idx_fpt]
  resp <- sapply(seq_along(idx_fpt),
                 function(i) sign(traces_dm_obj[i, idx_fpt[i]]))

  p_u <- mean(resp == 1)
  expected_fpt_desc <- c(mean(ts_fpt), sd(ts_fpt), p_u, 1 - p_u)
  names(expected_fpt_desc) <- c("mean", "sd", paste0("p_corr"), paste0("p_err"))

  expect_identical(summary_traces$fpt_desc, expected_fpt_desc)

  # Check print output snapshot
  expect_snapshot(print(summary_traces))

  # Check structure
  expect_identical(
    names(summary_traces),
    c("k", "add_x", "orig_model_class", "orig_prms", "prms_solve", "fpt_desc")
  )
})



test_that("summary.traces_dm_list works as expected", {

  some_model = dmc_dm(dt = .005, dx = .005)
  withr::local_preserve_seed()
  set.seed(1)
  traces_dm_list_obj <- simulate_traces(some_model, k = c(5, 10),
                                        add_x = c(TRUE, FALSE),
                                        sigma = c(0, 1))
  summary_traces_list <- summary(traces_dm_list_obj)


  # Check class
  expect_s3_class(summary_traces_list, "summary.traces_dm_list")

  # Check stored attributes
  expect_identical(summary_traces_list$k, c(comp = 5L, incomp = 10L))
  expect_identical(summary_traces_list$add_x, c(comp = TRUE, incomp = FALSE))

  # Check parameter matrices
  expect_identical(summary_traces_list$orig_prms,
                   some_model$flex_prms_obj$prms_matrix)

  temp = some_model$prms_solve
  temp = rbind(temp, temp)
  rownames(temp) = c("comp", "incomp")
  temp[,1] = c(0, 1)
  expect_identical(summary_traces_list$prms_solve, temp)

  # Check original model class consistency
  expect_identical(summary_traces_list$orig_model_class, class(some_model))

  # Check first passage time statistics matrix
  summaries <- lapply(traces_dm_list_obj, summary)
  fpt_desc_matrix <- t(sapply(summaries, `[[`, "fpt_desc"))
  expect_identical(summary_traces_list$fpt_desc, fpt_desc_matrix)

  # Check print output snapshot
  expect_snapshot(print(summary_traces_list))

  # Check structure
  expect_identical(
    names(summary_traces_list),
    c("k", "add_x", "orig_prms", "orig_model_class", "prms_solve", "fpt_desc")
  )
})

