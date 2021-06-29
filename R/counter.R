#' Counter R6 class to represent a counter
Counter <- R6::R6Class(
  "Counter",
  public = list(
    #' @field count number of times that something has
    #' happened
    count = 0L,

    #' @description
    #' Create a new Counter object.
    #' @return A new `Counter` object.
    initialize = function() {
    },

    #' @description
    #' Print a Counter object.
    #' @param ... required for compatibility with print()
    #' @return `self`
    print = function(...) {
      cat("<Counter>\n")
      print(list(count = self$count))
      return(invisible(self))
    },

    #' @description
    #' Add one to counter.
    #' @return `NULL`
    increment = function() {
      self$count <- self$count + 1L
      return(invisible(NULL))
    },

    #' @description
    #' Add one to counter.
    #' @return `self`
    chain_increment = function() {
      self$increment()
      return(invisible(self))
    },

    #' @description
    #' Add one to counter.
    #' @param .data piped dataset
    #' @return `.data`
    pipe_increment = function(.data) {
      self$increment()
      return(.data)
    },

    #' @description
    #' Reset counter to zero.
    #' @return `self`
    reset = function() {
      self$count <- 0L
      return(invisible(self))
    }
  )
)
