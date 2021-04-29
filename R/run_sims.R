run_sim <- function(sim_spec, est_specs, reporter, seed = NULL,
                    save_individual = TRUE) {
  simulation <- sim_spec$create(seed = seed)
  all_results <- lapply(est_specs, function(est_spec) {

    # set up sim
    sim_copy <- simulation$clone()
    sim_copy$estimator <- est_spec$create()
    sim_copy$reporter <- reporter$clone()

    # run it
    result <- try({
      sim_copy$run()
    }, silent = TRUE)

    if(inherits(result, "try-error")){
      message(sprintf("Error in sim: %s est: %s seed: %s",
              sim_copy$name,
              sim_copy$estimator$name,
              sim_copy$seed))

      msg <- attr(result,"condition")$message
      message(msg)

      return(NULL)
    }

    # save and return results
    if (save_individual) {
      sim_copy$reporter$save()
    }

    return(sim_copy$reporter$final_report)
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
                     save_individual = TRUE) {

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

  all_results <- future_lapply(seq_len(nrow(all_runs)), function(run_index) {
    spec_index <- all_runs[run_index, "spec_index"]
    sim_spec <- sim_specs[[spec_index]]

    run_sim(sim_spec, est_specs, reporter, save_individual = save_individual)
  }, future.seed = TRUE, future.stdout = NA)

  results <- do.call(c, all_results)

  return(results)
}
