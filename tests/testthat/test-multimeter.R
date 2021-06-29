test_that("mm_get_missing_meter() works", {
  maybe_na <- function(x, prob = 0.2) {
    n <- base::length(x)
    missing_of_type_x <- vctrs::vec_cast(NA, to = vctrs::vec_ptype(x))
    dplyr::if_else(stats::runif(n) < prob, missing_of_type_x, x)
  }

  # In pieces, to see what's going on
  missing_meter <- mm_get_missing_meter()
  library(dplyr)
  missing_meter$probe(starwars)

  set.seed(2021L + 06L + 28L)
  starwars2 <- starwars %>%
    mutate(homeworld = maybe_na(homeworld))

  missing_meter$probe(starwars2)
  expect_equal(nrow(missing_meter$comparison), 1L)

  # All together in one pipeline
  starwars2 <- starwars %>%
    missing_meter$reset() %>%
    missing_meter$probe() %>%
    mutate(homeworld = maybe_na(homeworld)) %>%
    missing_meter$probe()
  expect_equal(nrow(missing_meter$comparison), 1L)
})


test_that("mm_get_value_meter() gets unique combinations", {
  library(dplyr)
  mtcars_reduced <- mtcars %>%
    count(cyl) %>%
    select(-n)
  # Method 1 (on mtcars)
  m <- mm_get_value_meter()
  mtcars %>%
    m$probe(cyl) %>%
    mutate(square_error_from_five = (cyl - 5)^2) %>%
    m$probe(square_error_from_five)
  output1 <- m$comparison %>%
    select(-n)
  # Method 2 (on mtcars_reduced)
  m$reset()
  mtcars_reduced %>%
    m$probe(cyl) %>%
    mutate(square_error_from_five = (cyl - 5)^2) %>%
    m$probe(square_error_from_five)
  output2 <- m$comparison %>%
    select(-n)

  expect_equal(output1, output2)
})

test_that("mm_get_names_meter() works on identical dataframes", {
  m <- mm_get_names_meter()
  m$probe(mtcars)
  m$probe(mtcars)
  reference <- list(
    `before - after` = character(0),
    `same` = names(mtcars),
    `after - before` = character(0)
  )
  expect_equal(m$comparison, reference)
})

test_that("mm_get_names_meter() works on completely different", {
  m <- mm_get_names_meter()
  m$probe(mtcars)
  m$probe(dplyr::starwars)
  reference <- list(
    `before - after` = names(mtcars),
    `same` = character(0),
    `after - before` = names(dplyr::starwars)
  )
  expect_equal(m$comparison, reference)
})
