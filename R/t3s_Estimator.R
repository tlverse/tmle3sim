#' @import data.table
#' @importFrom R6 R6Class
#' @export
t3s_Estimator <- R6Class("t3s_Estimator",
  public = list(
    initialize = function(params = NULL, ...) {
      # add default parameters if missing
      if (is.null(params$name)) {
        params$name <- class(self)[1]
      }

      private$.params <- params

      private$.uuid <- digest(self$params)
    },
    estimate = function(...) {

    },
    print = function() {
      header <- sprintf("A %s estimator\n", self$name)
      message(header)
    }
  ),
  active = list(
    params = function() {
      return(private$.params)
    },
    simulation = function(simulation = NULL) {
      if (!is.null(simulation)) {
        private$.simulation <- simulation
      }
      return(private$.simulation)
    },
    name = function() {
      return(private$.params$name)
    },
    uuid = function() {
      return(private$.uuid)
    }
  ),
  private = list(
    .params = NULL,
    .simulation = NULL,
    .uuid = NULL
  )
)
