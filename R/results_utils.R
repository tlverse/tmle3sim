#' @import origami
#' @export
load_results <- function(path = "Results") {
  results_files <- dir(path, pattern = "rdata$", full.names = TRUE)
  # TODO: make this smarter (currently just gets whatever first object is)
  all_results <- lapply(results_files, function(results_file) {
    var <- load(results_file)
    get(var[1])
  })

  return(all_results)
}


#' @export
format_results <- function(results, invert = TRUE, combine = TRUE,
                           combine_control = list()) {
  # ported from origami
  # maybe backport this function there
  if (invert) {
    results <- apply(do.call(rbind, results), 2, as.list)
  }
  if (combine) {
    results <- do.call(
      origami::combine_results,
      c(
        list(results = results),
        combine_control
      )
    )
  }

  return(results)
}

#' @export
mc_wald <- function(x, alpha = 0.05) {
  value <- mean(x)
  nc_mc <- length(x)
  se <- sd(x) / sqrt(nc_mc)
  lower <- value - qnorm(1 - alpha / 2) * se
  upper <- value + qnorm(1 - alpha / 2) * se
  list(
    value = value, se = se,
    lower = lower, upper = upper,
    n_mc = nc_mc
  )
}

#' @export
performance_summary <- function(results_dt, conditions, metrics) {
  suppressWarnings(long <- melt(results_dt,
    id = conditions,
    measure = metrics,
    variable.name = "metric"
  ))

  perf_summary <- long[, mc_wald(value), by = c(conditions, "metric")]
  class(perf_summary) <- c("t3s.ps", class(perf_summary))
  return(perf_summary)
}

#' @export
plot_ts_ps <- function(perf_summary) {
  ts_plot <- ggplot(perf_summary, aes(
    x = step, y = value,
    ymin = lower, ymax = upper
  )) +
    geom_line(aes(color = name)) +
    geom_ribbon(aes(fill = name), alpha = 0.1) +
    theme_bw() +
    xlab("Time") +
    scale_color_discrete("Method") +
    scale_fill_discrete("Method")

  if (length(unique(perf_summary$metric)) == 1) {
    ts_plot <- ts_plot +
      ylab(perf_summary$metric[1])
  } else {
    ts_plot <- ts_plot +
      facet_wrap(~metric,
        ncol = 1,
        scales = "free_y"
      ) +
      ylab("Performance")
  }
}

# TODO: add options for ci, other things
#' @export
plot.t3s.ps <- function(perf_summary, ...) {
  ps_plot <- plot_ts_ps(perf_summary)

  print(ps_plot)
}

#' @export
missing_results <- function(result_path="Results", log_path = "Logs"){
  results <- dir("Results")
  logs <- dir("Logs")
  expected_results <- gsub("log","results",logs)
  expected_results <- gsub("txt","rdata",expected_results)
  missing_results <- setdiff(expected_results, results)

  return(missing_results)
}
