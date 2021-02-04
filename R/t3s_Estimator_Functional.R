#' @importFrom R6 R6Class
#' @export
t3s_Estimator_Functional <- R6Class("t3s_Estimator_Functional",
                                     inherit=t3s_Estimator,
                                     public = list(
                                       initialize = function(params = NULL, ..., est_fun){
                                         if(is.null(params)){
                                           params = list()
                                         }
                                         params$est_fun = est_fun
                                         private$.est_fun = est_fun
                                         super$initialize(params = params, ...)
                                       },
                                       estimate = function(){
                                         est_fun_args <- self$params
                                         est_fun_args$simulation = self$simulation
                                         result <- do.call(self$est_fun, est_fun_args)

                                         return(result)
                                       }
                                     ),
                                     active = list(
                                       est_fun = function(){
                                         return(private$.est_fun)
                                       }
                                     ),
                                     private = list(
                                       .est_fun = NULL
                                     )
)

est_from_fun = function(est_fun, ...){
  return(t3s_Estimator_Functional$new(..., est_fun=est_fun))
}
