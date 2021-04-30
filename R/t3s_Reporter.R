#' @import data.table
#' @importFrom R6 R6Class
#' @export
t3s_Reporter <- R6Class("t3s_Reporter",
  public = list(
    initialize = function(params = list()) {
      if (is.null(params$report_names)) {
        params$report_names <- TRUE
      }

      if (is.null(params$report_uuids)) {
        params$report_uuids <- TRUE
      }

      if(is.null(params$log)){
        params$log <- TRUE
      }

      if(params$log && is.null(params$log_path)){
          params$log_path <- "Logs"
      }

      if (is.null(params$path)) {
        params$path <- "Results"
      }

      private$.params <- params
      private$.reports <- list()
    },
    report = function(...) {
      new_report <- self$simulation$last_estimate
      if (self$simulation$n_steps > 1) {
        new_report$step <- self$simulation$step
      }

      private$.reports <- c(private$.reports, list(new_report))
    },
    make_final = function(...) {
      combined <- rbindlist(self$reports)
      sim_params <- self$simulation$params

      sp_dt <- data.table()

      if (self$params$report_names) {
        sp_dt$simulation_name <- self$simulation$name
        sp_dt$estimator_name <- self$simulation$estimator$name
      }

      if (self$params$report_uuids) {
        sp_dt$simulation_uuid <- self$simulation$uuid
        sp_dt$estimator_uuid <- self$simulation$estimator$uuid
      }

      sp_dt$seed <- self$simulation$seed

      combined <- cbind(sp_dt, combined)

      private$.final_report <- combined
    },
    save = function() {
      path <- self$params$path

      if (!dir.exists(path)) {
        dir.create(path)
      }
      filename <- self$filename

      filename <- file.path(path, filename)
      report <- self$final_report
      save(report, file = filename)
    },
    print = function() {
      header <- sprintf("A %s reporter\n", self$name)
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
      result <- private$.name
      if (is.null(result)) {
        result <- class(self)[1]
      }
    },
    reports = function() {
      return(private$.reports)
    },
    final_report = function() {
      return(private$.final_report)
    },
    filename = function() {
      filename <- sprintf(
        "results_%s.rdata",
        self$simulation$key
      )

      return(filename)
    }
  ),
  private = list(
    .params = NULL,
    .simulation = NULL,
    .name = NULL,
    .reports = NULL,
    .final_report = NULL
  )
)
