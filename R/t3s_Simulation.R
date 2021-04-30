#' @import data.table
#' @importFrom digest digest
#' @importFrom R6 R6Class
#' @export
t3s_Simulation <- R6Class("t3s_Simulation",
  public = list(
    initialize = function(params = NULL,
                          estimator = NULL, reporter = NULL,
                          seed = NULL,
                          ...) {

      # add default parameters if missing
      if (is.null(params$name)) {
        params$name <- class(self)[1]
      }

      if (is.null(params$n_steps)) {
        params$n_steps <- 1
      }

      # initialize private fields
      private$.params <- params
      private$.estimator <- estimator
      private$.reporter <- reporter
      private$.step <- 0

      if (is.null(seed)) {
        seed <- as.integer(runif(1, 0, 2 * (10^9)))
      }
      private$.seed <- seed
      set.seed(seed)


      # register simulation with other components
      estimator$simulation <- self
      reporter$simulation <- self

      # compute and store uuid
      private$.uuid <- digest(self$params)
    },
    sample = function() {

    },
    run_step = function() {
      private$.step <- self$step + 1
      private$.last_sample <- self$sample()
      private$.last_estimate <- self$estimator$estimate()
      self$reporter$report()
    },
    run = function() {
      private$.start_time <- proc.time()
      while (self$step < self$n_steps) {
        if (getOption("tmle3sim.verbose")) {
          msg <- sprintf(
            "Running %s step %d of %d\n",
            self$name,
            self$step,
            self$n_steps
          )
          message(msg)
        }


        self$run_step()
      }

      private$.runtime <- (proc.time()-private$.start_time)[[3]]
      self$reporter$make_final()
    },
    print = function() {
      header <- sprintf("A %s simulation\n", self$name)
      message(header)
      # print(self$params)
      footer <- sprintf("seed=%s \n", self$seed)
      message(footer)
    }
  ),
  active = list(
    params = function() {
      return(private$.params)
    },
    full_params = function() {
      result <- list(
        simulation = self$params,
        estimator = self$estimator$params
      )

      return(result)
    },
    estimator = function(estimator = NULL) {
      if (!is.null(estimator)) {
        estimator$simulation <- self
        private$.estimator <- estimator
      }
      return(private$.estimator)
    },
    reporter = function(reporter = NULL) {
      if (!is.null(reporter)) {
        reporter$simulation <- self
        private$.reporter <- reporter
      }

      return(private$.reporter)
    },
    name = function() {
      return(private$.params$name)
    },
    step = function() {
      return(private$.step)
    },
    n_steps = function() {
      return(private$.params$n_steps)
    },
    seed = function() {
      return(private$.seed)
    },
    uuid = function() {
      return(private$.uuid)
    },
    last_sample = function() {
      return(private$.last_sample)
    },
    last_estimate = function() {
      return(private$.last_estimate)
    },
    runtime = function() {
      return(private$.runtime)
    },
    key = function(){
      paste(self$uuid,
            self$estimator$uuid,
            self$seed,
            sep="_")
    }
  ),
  private = list(
    .params = NULL,
    .estimator = NULL,
    .start_time = NULL,
    .runtime = NULL,
    .reporter = NULL,
    .seed = NULL,
    .step = NULL,
    .uuid = NULL,
    .last_sample = NULL,
    .last_estimate = NULL
  ),
)
