#!/usr/bin/env Rscript

# Runs a single job
# Requires four arguments: trait, D, B, path

args=commandArgs(TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) !=4) {
  stop("Four arguments must be supplied", call.=FALSE)
} else {
  trait  = as.character(args[1])
  D  = as.numeric(args[2])
  B  = as.numeric(args[3])
  path = as.character(args[4])
  }

library(dplyr)

source("R/my_model.R")
run_my_model(trait, D, B, path)
