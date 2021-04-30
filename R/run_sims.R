run_sim <- function(sim_spec, est_specs, reporter, seed = NULL,
                    save_individual = TRUE, sim_log = FALSE, root_log="ROOT") {
  simulation <- sim_spec$create(seed = seed)
  all_results <- lapply(est_specs, function(est_spec) {

    # set up sim
    sim_copy <- simulation$clone()
    sim_copy$estimator <- est_spec$create()
    sim_copy$reporter <- reporter$clone()

    if(sim_log){
      #TODO: make this a package option or something
      log_path <- "Logs"
      if (!dir.exists(log_path)) {
        dir.create(log_path)
      }

      log_file <- sprintf(
        "log_%s.txt",
        sim_copy$key
      )

      log_file <- file.path(log_path, log_file)

      flog.logger(sim_copy$key, INFO, appender=appender.file(log_file))
    } else {
      log_file = "console"
      flog.logger(sim_copy$key, INFO)
    }

    log_message <- sprintf("[%s] Running pid: %s sim: %s est: %s seed: %s log: %s",
                           Sys.time(),
                           Sys.getpid(),
                           sim_copy$name,
                           sim_copy$estimator$name,
                           sim_copy$seed,
                           log_file)
    flog.info(log_message, name = root_log)
    if(sim_log){
      # log to sim specific file too
      flog.info(log_message, name = sim_copy$key)
    }

    # run the simulation with logging
    result <- try_with_logs(sim_copy$run(), context = sim_copy$key)


    if(inherits(result, "try-error")){
      result <- NULL
    } else {
      # save and return results
      if (save_individual) {
        sim_copy$reporter$save()
      }

      sim_copy$reporter$final_report
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
  root_logfile <- "simlog.txt"
  if(file.exists(root_logfile)){
    file.remove(root_logfile)
  }
  all_results <- future_lapply(seq_len(nrow(all_runs)), function(run_index, root_logfile) {
    spec_index <- all_runs[run_index, "spec_index"]
    sim_spec <- sim_specs[[spec_index]]
    flog.logger(root_logfile, INFO, appender=appender.file(root_logfile))
    run_sim(sim_spec, est_specs, reporter,
            save_individual = save_individual, sim_log = TRUE, root_log = root_logfile)
  }, root_logfile = root_logfile, future.seed = TRUE, future.stdout = FALSE)

  results <- do.call(c, all_results)

  return(results)
}
