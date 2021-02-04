library(devtools)
library(tmle3)
library(ggplot2)
library(future)
load_all()

# define simulation / DGD
# TODO: maybe DGD should be separate object
example_dgd <- function(n, mean, sd, ...) {
  list(x = rnorm(n, mean, sd))
}

params <- list(
  name = "rnorm_sim",
  n = 1000,
  mean = 0,
  sd = 1
)

sim_spec <- make_spec(t3s_Simulation_Functional,
  params = params,
  sim_fun = example_dgd
)



example_est <- function(simulation, ...) {
  data <- simulation$last_sample
  result <- list(
    xbar = mean(data$x),
    sigma = sd(data$x)
  )

  return(result)
}

example_est2 <- function(simulation, ...) {
  data <- simulation$last_sample
  result <- list(
    xbar = median(data$x),
    sigma = IQR(data$x)
  )

  return(result)
}

est_spec1 <- make_spec(t3s_Estimator_Functional,
  params = list(name = "mean"),
  est_fun = example_est
)

est_spec2 <- make_spec(t3s_Estimator_Functional,
  params = list(name = "median"),
  est_fun = example_est2
)


sim_specs <- sim_spec
est_specs <- list(est_spec1, est_spec2)
reporter <- t3s_Reporter$new()

plan(multicore, workers = 2)
results <- run_sims(sim_spec, est_specs, reporter, n_runs = 1e3)
results_df <- rbindlist(results)
results_sum <- results_df[, list(mean(xbar), sd(xbar)), by = list(estimator_name)]
print(results_sum)
