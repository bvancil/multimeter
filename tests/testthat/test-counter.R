test_that("Counter initializes as expected", {
  expect_equal(Counter$new()$count, 0L)
})

test_that("Counter counts via $increment()", {
  c1 <- Counter$new()
  c1$increment()
  c1$increment()
  c1$increment()
  expect_equal(c1$count, 3L)
})

test_that("Counter counts via $chain_increment()", {
  expect_equal(Counter$new()$chain_increment()$chain_increment()$chain_increment()$count, 3L)
})

test_that("Counter counts via $pipe_incremement()", {
  c1 <- Counter$new()
  mtcars %>%
    c1$pipe_increment() %>%
    c1$pipe_increment() %>%
    c1$pipe_increment()
  expect_equal(c1$count, 3L)
})

test_that("Counter $reset() works", {
  c1 <- Counter$new()
  c1$increment()
  c1$reset()
  expect_equal(c1$count, 0L)
})
