#' @import data.table
#' @importFrom R6 R6Class
#' @export
t3s_Spec <- R6Class("t3s_Spec",
                    public = list(
                      initialize = function(obj_class, params, ...){
                        private$.obj_class = obj_class
                        private$.params = params
                        private$.other_args = list(...)
                      },
                      create = function(...){
                        # start with specced arglist
                        args <- self$other_args
                        
                        # add any new args from create (e.g. seed)
                        new_args = list(...)
                        args[names(new_args)]=new_args[names(new_args)]
                        
                        # add in params
                        args$params <- self$params
                        do.call(self$obj_class$new, args)
                        
                      }
                    ),
                    active = list(
                      obj_class = function(){
                        return(private$.obj_class)
                      },
                      params = function(){
                        return(private$.params)
                      },
                      other_args = function(){
                        return(private$.other_args)
                      }
                    ),
                    private = list(
                      .obj_class = NULL,
                      .params = NULL,
                      .other_args = NULL
                    )
)
                        
make_spec <- t3s_Spec$new
