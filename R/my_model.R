
run_my_model <- function(trait, D, B, path) {
  
  # Create model output. Obviously this is a simplistic toy example
  x <- tibble(trait, D, B)
  
  # make sure folder exists
  if(dirname(path) != ".")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # save output
  saveRDS(x, path)
}
