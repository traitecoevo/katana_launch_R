
launch_task_i_katana <- function(par_set, sleep=0, dry_run=FALSE, pbs.whisker = "launch/pbs_katana.whisker") {
  file <- write_pbs(par_set, pbs.whisker = pbs.whisker)
  res <- qsub(file, dry_run=dry_run, verbose=TRUE, sleep=sleep)

  res
}

write_pbs <- function(par_set, pbs.whisker) {
  template <- readLines(pbs.whisker)
  str <- whisker::whisker.render(template, par_set)
  file <- paste0(par_set$path, ".pbs")
  dir.create(dirname(file), FALSE, TRUE)
  writeLines(str, file)
  file
}


qsub <- function(pbs_filenames, dry_run=TRUE, verbose=TRUE, sleep=0) {
  if (dry_run) {
    system2 <- function(command, args, stdout=TRUE) {
      message(paste(command, args, "(Dry run)"))
    }
  }
  pbs_ids <- vector("list", length=length(pbs_filenames))
  for (i in seq_along(pbs_filenames)) {
    if (verbose) {
      message("Launching ", pbs_filenames[[i]])
    }
    pbs_ids[[i]] <- system2("qsub", pbs_filenames[[i]], stdout=TRUE)
  }
  if(sleep > 0){
    Sys.sleep(sleep)
  }
  ## TODO: Throw an error if the job was refused.
  invisible(pbs_ids)
}

# Delete jobs from queue
qdel <-function(i, txt = ".katana.science.unsw.edu.au"){
   msg <- paste(paste0(i, txt), collapse= " ")
   system2("qdel", msg, stdout=TRUE)
}

check_status <- function(x) {
  
  status <- rep("none", length(x))
  
  # Simulation finished if rds file exists
  files <- paste0(x, ".rds")
  has_started <- file.exists(files)
  status[has_started] <- "finished"
  
  
  # Simulation started if log file exists
  files <- paste0(x, ".log")
  has_started <- file.exists(files) & status!="finished"
  status[has_started] <- "started"
  
  # # simulation finished if last line of log file contains signal. Only check files that have started
  # signal <- "Ending log"
  # i <- files[has_started] %>% purrr::map_lgl(~readLines(.x) %>% last() %>% grepl(signal, .))
  # names_finished <- files[has_started][i]
  # status[files %in% names_finished] <- "finished"
  
  status
}

check_grid_status <- function(file){
  read_csv(file, show_col_types = FALSE) %>% 
    mutate(
      status = check_status(path)
    )
}
