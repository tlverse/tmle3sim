library(devtools)
library(tmle3)

load_all()

# define simulation / DGD
# TODO: maybe DGD should be separate object
example_dgd <- function(n, mean, sd, ...) {
  list(x = rnorm(n, mean, sd))
}

params <- list(
  n = 1000,
  mean = 0,
  sd = 1
)

simulation <- sim_from_fun(example_dgd,
  params = params,
  vebose = TRUE
)

# define estimation strategy

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

mean_est <- est_from_fun(example_est, params = list(name = "mean_estimator"))
median_est <- est_from_fun(example_est2, params = list(name = "median_estimator"))

t3s_Reporter$new()

simulations <- list(simululation)
simulation$estimator <-
  simulation$reporter <-
  simulation$full_params
# debugonce(simulation$reporter$report)
# debugonce(simulation$reporter$make_final)

simulation$run()
debugonce(simulation$reporter$save)
simulation$reporter$save()
