.onLoad <- function(...){
  options(tmle3sim.stacktrace = TRUE)
  options(tmle3sim.dumpfile = FALSE)
  options(tmle3sim.verbose = FALSE)
}

.tmle3sim_env = new.env()