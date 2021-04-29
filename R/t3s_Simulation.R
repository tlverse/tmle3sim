#' @import data.table
#' @importFrom digest digest
#' @importFrom R6 R6Class
#' @export
t3s_Simulation <- R6Class("t3s_Simulation",
  public = list(
    initialize = function(params = NULL,
                          estimator = NULL, reporter = NULL,
                          seed = NULL, verbose = FALSE,
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

      private$.verbose <- verbose

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
      if(self$reporter$params$log){
        log_path <- self$reporter$params$log_path
        if (!dir.exists(log_path)) {
          dir.create(log_path)
        }

        log_file <- sprintf(
          "log_%s_%s_%s.txt",
          self$uuid,
          self$estimator$uuid,
          self$seed
        )

        message(sprintf("Logging pid: %s sim: %s est: %s seed: %s @ %s",
                Sys.getpid(),
                self$name,
                self$estimator$name,
                self$seed,
                log_file))
        sink(file.path(log_path, log_file))
      }

      result <- tryLog({
        while (self$step < self$n_steps) {
          if (self$verbose) {
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

        self$reporter$make_final()
      })

      if(self$reporter$params$log){
        sink()
      }

      return(result)
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
    verbose = function() {
      return(private$.verbose)
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
    }
  ),
  private = list(
    .params = NULL,
    .estimator = NULL,
    .reporter = NULL,
    .seed = NULL,
    .step = NULL,
    .verbose = NULL,
    .uuid = NULL,
    .last_sample = NULL,
    .last_estimate = NULL
  ),
)
