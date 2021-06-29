test_that("History initialization works", {
  expect_identical(History$new()$values, list())
})


test_that("History logging works", {
  values_to_log <- as.list(runif(10L))
  h <- History$new()
  for (val in values_to_log) {
    h$log(val)
  }
  expect_identical(h$values, values_to_log)
})

test_that("History chain logging works", {
  values_to_log <- as.list(runif(10L))
  h <- History$new()
  for (val in values_to_log) {
    h <- h$chain_log(val)
  }
  expect_identical(h$values, values_to_log)
})

test_that("History pipe logging works", {
  h <- History$new()
  val <- runif(1L)
  val %>%
    h$pipe_log() %>%
    h$pipe_log() %>%
    h$pipe_log()
  expect_identical(h$values, as.list(rep(val, times = 3L)))
})

test_that("History pipe logging variant works", {
  h <- History$new()
  val <- runif(1L)
  h$pipe_log(h$pipe_log(h$pipe_log(val)))
  expect_identical(h$values, as.list(rep(val, times = 3L)))
})
