
launch_task_i_katana <- function(par_set, sleep=0, dry_run=TRUE, pbs.whisker = "launch/pbs_katana.whisker") {
  file <- write_pbs(par_set, pbs.whisker = pbs.whisker)
  res <- qsub(file, dry_run=dry_run, verbose=TRUE, sleep=sleep)
  #dat <- process_pbs(par_set, res)
  #append_jobfile(dat, file.path(path, "_jobs.csv"))
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
    system2 <- function(command, args, ...) {
      message(paste(command, args, ...))
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

append_jobfile <- function(dat, jobfile="pbs_jobs.csv") {
  if (file.exists(jobfile)) {
    prev <- read.csv(jobfile, stringsAsFactors=FALSE)
    ## *Replace* existing ids.
    v <- c("id", "pbs_id")
    hash_prev <- apply(prev[v], 1, paste, collapse="\r")
    hash_dat <- apply(dat[v], 1, paste, collapse="\r")
    prev <- prev[!(hash_prev %in% hash_dat),]
  } else {
    prev <- NULL
  }
  write.csv(rbind(prev, dat), jobfile, row.names=FALSE)
}

process_pbs <- function(id, pbs) {
  data.frame(id=id,
             pbs_id=as.integer(sub("\\..+$", "", pbs)),
             stringsAsFactors=FALSE)
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
