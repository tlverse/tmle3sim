#' @importFrom R6 R6Class
#' @export
t3s_Simulation_Functional <- R6Class("t3s_Simulation_Functional",
  inherit = t3s_Simulation,
  public = list(
    initialize = function(params = NULL, ..., sim_fun) {
      if (is.null(params)) {
        params <- list()
      }
      params$sim_fun <- sim_fun
      private$.sim_fun <- sim_fun
      super$initialize(params = params, ...)
    },
    sample = function() {
      sim_fun_args <- self$params
      sim_fun_args$simulation <- self
      result <- do.call(self$sim_fun, sim_fun_args)

      return(result)
    }
  ),
  active = list(
    sim_fun = function() {
      return(private$.sim_fun)
    }
  ),
  private = list(
    .sim_fun = NULL
  )
)

sim_from_fun <- function(sim_fun, ...) {
  return(t3s_Simulation_Functional$new(..., sim_fun = sim_fun))
}
