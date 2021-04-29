#' @importFrom tryCatchLog tryLog
run_sim <- function(sim_spec, est_specs, reporter, seed = NULL,
                    save_individual = TRUE, log = TRUE, stdout=NULL) {
  simulation <- sim_spec$create(seed = seed)
  all_results <- lapply(est_specs, function(est_spec) {

    # set up sim
    sim_copy <- simulation$clone()
    sim_copy$estimator <- est_spec$create()
    sim_copy$reporter <- reporter$clone()

    if(log){
      #TODO: make this a package option or something
      log_path <- "Logs"
      if (!dir.exists(log_path)) {
        dir.create(log_path)
      }

      log_file <- sprintf(
        "log_%s_%s_%s.txt",
        sim_copy$uuid,
        sim_copy$estimator$uuid,
        sim_copy$seed
      )

      log_message <- sprintf("[%s] Running pid: %s sim: %s est: %s seed: %s log: %s",
                      Sys.time(),
                      Sys.getpid(),
                      sim_copy$name,
                      sim_copy$estimator$name,
                      sim_copy$seed,
                      log_file)

      if(!is.null(stdout)){
        writeLines(log_message, stdout)
      } else{
        message(log_message)
      }
      sink(file.path(log_path, log_file))
      message(log_message)
    }

    # run it
    result <- tryLog({
      sim_copy$run()

      # save and return results
      if (save_individual) {
        sim_copy$reporter$save()
      }

      sim_copy$reporter$final_report
    })


    if(inherits(result, "try-error")){
      result <- NULL
    }

    if(log){
      sink()
    }


    return(result)

  })

  if (length(est_specs) == 1) {
    all_results <- list(all_results)
  }

  return(all_results)
}

#' @export
#' @import future.apply
run_sims <- function(sim_specs,
                     est_specs,
                     reporter = NULL,
                     n_runs = 100,
                     save_individual = TRUE,
                     log = TRUE) {

  # listify singular specs
  if (inherits(sim_specs, "t3s_Spec")) {
    sim_specs <- list(sim_specs)
  }

  if (inherits(est_specs, "t3s_Spec")) {
    est_specs <- list(est_specs)
  }

  if (is.null(reporter)) {
    reporter <- t3s_Reporter$new()
  }

  all_runs <- expand.grid(spec_index = seq_along(sim_specs), run = 1:n_runs)

  all_results <- future_lapply(seq_len(nrow(all_runs)), function(run_index, stdout) {
    spec_index <- all_runs[run_index, "spec_index"]
    sim_spec <- sim_specs[[spec_index]]

    run_sim(sim_spec, est_specs, reporter,
            save_individual = save_individual, log = log, stdout = stdout)
  }, stdout=stdout(), future.seed = TRUE, future.stdout = FALSE)

  results <- do.call(c, all_results)

  return(results)
}
