#' @import origami
#' @export
load_results <- function(path = "Results", combine = TRUE){
  results_files <- dir(path, pattern="rdata$", full.names = TRUE)
  # TODO: make this smarter (currently just gets whatever first object is)
  all_results <- lapply(results_files, function(results_file){
    var = load(results_file); get(var[1])
  })

  # TODO: make this smarter (combine lists of results and invert)
  if(combine){
    results <- origami::combine_results(list(x=all_results))$x
  } else {
    results <- all_results
  }
  return(results)
}
