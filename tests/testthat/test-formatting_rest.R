test_that("print.traces_dm_list works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))
  traces <- simulate_traces(a_model, k = 2, seed = 1)

  expect_snapshot(
    print(traces)
  )
})


test_that("print.traces_dm_list works as expected", {
  a_model <- readRDS(test_path("fixtures", "ssp.rds"))
  traces <- simulate_traces(a_model, k = 5, seed = 1)
  traces <- unpack_traces(traces, unpack = F, conds = "comp")

  expect_snapshot(
    print(traces, print_steps = 6, print_k = 3)
  )
})
