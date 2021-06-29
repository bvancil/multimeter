#' History R6 class to represent historical values
History <- R6::R6Class(
  "History",
  public = list(
    #' @field values list of stored values
    values = NULL,

    #' @description
    #' Create a new History object.
    #'
    #' @return A new `History` object.
    initialize = function() {
      self$values <- list()
    },

    #' @description
    #' Print summary of `Counter` object
    #' @param ... unused
    print = function(...) {
      cat("<History>\n")
      print(self$values)
      return(invisible(self))
    },

    #' @description
    #' Store a value
    #'
    #' @param val value to add to history
    #' @return `NULL`
    log = function(val) {
      self$values <- c(self$values, val)
      return(invisible(NULL))
    },

    #' @description
    #' Store a value
    #'
    #' @param val value to add to history
    #' @return `self`
    chain_log = function(val) {
      self$log(val)
      return(invisible(self))
    },

    #' @description
    #' Store a value
    #'
    #' @param val value to add to history
    #' @return `val`
    pipe_log = function(val) {
      force(val)
      self$log(val)
      return(val)
    },


    #' @description
    #' Clear history
    #'
    #' @param x pipeline value passed in (Optional)
    reset = function(x) {
      self$values <- list()
      if (missing(x)) {
        return(invisible(self))
      } else {
        return(x)
      }
    }
  )
)
