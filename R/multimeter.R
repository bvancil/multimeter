# Multimeter -------------------------------------------

#' Multimeter R6 class to represent a multimeter type
#'
#' A new Multimeter can insert probes in various stages
#' of a data pipeline, allowing you to compare output
#' before and after a change. To facilitate use with large
#' datasets for which you might only wish to store a summary
#' of the data, a Multimeter object can take a `transform`
#' function that transforms the data before storing it, and
#' a `compare` function to point out salient differences.
#'
#' @export
Multimeter <- R6::R6Class(
  "Multimeter",
  public = list(
    #' @field transform function of dataset
    transform = NULL,
    #' @field compare function of before and after
    #' transformed datasets
    compare = NULL,
    #' @field before transformed "before" dataset
    before = NULL,
    #' @field after transformed "after" dataset
    after = NULL,
    #' @field comparison result of `compare` on `before`
    #' and `after`
    comparison = NULL,

    #' @description
    #' Create a new Multimeter object.
    #' @param transformer function of dataset to store
    #' @param comparator function of transformed dataset to
    #' compare
    #' @return A new `Multimeter` object.
    initialize = function(transformer, comparator) {
      self$transform <- transformer
      self$compare <- comparator
    },

    #' @description
    #' Print summary of `Multimeter` object
    #' @param ... unused
    print = function(...) {
      cat("<Multimeter>\n")
      print(list(
        transform = self$transform,
        compare = self$compare,
        before = self$before,
        after = self$after,
        comparison = self$comparison
      ))
    },

    #' @description
    #' Insert a probe at this stage of data pipeline.
    #'
    #' Because this operation returns the first argument, it
    #' may be used in magrittr or base R pipelines.
    #'
    #' @param .data dataset tibble or data.frame
    #' @param ... other arguments passed on to
    #' `self$transform`
    #' @param .print_comparison logical whether to print a
    #' comparison on the second `probe`. Ignored otherwise.
    #' Default: `TRUE`
    #' @return `.data`
    probe = function(.data, ..., .print_comparison = TRUE) {
      force(.data)
      private$num_probes <- private$num_probes + 1L
      if (private$num_probes > 2L) {
        base::stop("The following Multimeter has been run more than twice. To measure the state of the pipeline, assuming the pipeline probe is called `check_something`, you must insert `check_something$probe()` twice in the {magrittr} pipeline.")
      }
      if (private$num_probes == 1L) {
        self$before <- self$transform(.data, ...)
      } else if (private$num_probes == 2L) {
        self$after <- self$transform(.data, ...)
        self$comparison <- self$compare(self$before, self$after)
        if (.print_comparison) {
          print(self$comparison)
        }
      }
      return(.data)
    },

    #' @description
    #' Delete intermediate stored transformed before/after
    #' datasets to save memory.
    #' @param .data dataset Optional if used in a data pipeline
    #' @return `.data` (or `NULL` if `.data` is missing)
    save_memory = function(.data) {
      self$before <- NULL
      self$after <- NULL
      if (base::missing(.data)) {
        return(invisible(self))
      }
      .data
    },

    #' @description
    #' Reset multimeter for repeated use
    #' @param .data dataset Optional if used in a data pipeline
    #' @return `.data` (or `NULL` if `.data` is missing)
    reset = function(.data) {
      private$num_probes <- 0L
      self$comparison <- NULL
      self$before <- NULL
      self$after <- NULL
      if (base::missing(.data)) {
        return(invisible(self))
      }
      return(.data)
    }
  ),
  private = list(
    num_probes = 0L
  )
)


# Multimeter examples ------------------------------

#' Simple meter that does not transform its input and
#' performs no comparison of results other than returning
#' them.
#'
#' @return `Multimeter` instance
#' @export
#'
#' @examples
#' if (interactive()) {
#'   m <- mm_get_identity_meter()
#'   m$probe(1L) == 1L
#'   m$probe(2L) == 2L
#'   m$comparison
#'   print(m)
#' }
mm_get_identity_meter <- function() {
  Multimeter$new(
    transformer = identity,
    comparator = function(before, after) list(before = before, after = after)
  )
}

#' Compare missingness fractions in each column of a data
#' frame and print any increases in missingness.
#'
#' This is useful after `mutate` operations but may give
#' false positives after a `summarize`.
#'
#' @return a new Multimeter tuned to look for missing values
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Function to randomly create NAs in a variable
#'   maybe_na <- function(x, prob = 0.2) {
#'     n <- base::length(x)
#'     missing_of_type_x <- vctrs::vec_cast(NA, to = vctrs::vec_ptype(x))
#'     dplyr::if_else(stats::runif(n) < prob, missing_of_type_x, x)
#'   }
#'   # In pieces, to see what's going on
#'   missing_meter <- mm_get_missingness_meter()
#'   library(dplyr)
#'   missing_meter$probe(starwars)
#'   set.seed(2021L + 06L + 28L)
#'   starwars2 <- starwars %>%
#'     mutate(homeworld = maybe_na(homeworld))
#'   missing_meter$probe(starwars2)
#'   print(missing_meter$comparison)
#'
#'   # All together in one pipeline
#'   starwars2 <- starwars %>%
#'     missing_meter$reset() %>%
#'     missing_meter$probe() %>%
#'     mutate(homeworld = maybe_na(homeworld)) %>%
#'     missing_meter$probe()
#'   print(missing_meter$comparison)
#' }
mm_get_missing_meter <- function() {
  Multimeter$new(
    transformer = function(dataset) {
      frac_missing <- function(x) base::mean(base::is.na(x))
      missing_data <- purrr::map_dbl(dataset, frac_missing)
      tibble::tibble(
        column = names(missing_data),
        frac_missing = missing_data
      )
    },
    comparator = function(before, after) {
      dplyr::inner_join(
        before,
        after,
        by = "column",
        suffix = c("_before", "_after")
      ) %>%
        dplyr::filter(frac_missing_before < frac_missing_after)
    }
  )
}

#' Track changes in columns provided to $probe() function
#'
#' @return `Multimeter` instance
#' @export
#'
#' @examples
#' if (interactive()) {
#'   m <- mm_get_state_capture_meter()
#'   library(dplyr)
#'   mtcars2 <- mtcars %>%
#'     m$probe(cyl) %>%
#'     mutate(square_error_from_five = (cyl - 5)^2) %>%
#'     m$probe(square_error_from_five)
#'   print(m$comparison)
#' }
mm_get_value_meter <- function() {
  Multimeter$new(
    transformer = function(dataset, ...) {
      dplyr::select(dataset, ...)
    },
    comparator = function(before, after) {
      dplyr::bind_cols(
        before,
        after,
      ) %>%
        dplyr::count(dplyr::across())
    }
  )
}

#' Track column names before and after a transformation
#'
#' @return `Multimeter` instance
#' @export
#'
#' @examples
#' if (interactive()) {
#'   m <- mm_get_names_meter()
#'   library(dplyr)
#'   mtcars2 <- mtcars %>%
#'     m$probe() %>%
#'     mutate(square_error_from_five = (cyl - 5)^2) %>%
#'     m$probe()
#'   print(m$comparison)
#' }
mm_get_names_meter <- function() {
  Multimeter$new(
    transformer = base::names,
    comparator = function(before, after) {
      list(
        `before - after` = base::setdiff(before, after),
        `same` = base::intersect(before, after),
        `after - before` = base::setdiff(after, before)
      )
    }
  )
}
