#!/usr/bin/env Rscript

# Runs a single assembly
# Requires three arguments: trait_name, disturbance regime, value of trade-off

args=commandArgs(TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) !=4) {
  stop("Four arguments must be supplied", call.=FALSE)
} else {
  trait  = as.character(args[1])
  d  = as.numeric(args[2])
  b  = as.numeric(args[3])
  path = as.character(args[4])
  }

library(plant)
source("R/assembly.R")
source("R/plant.R")

info <- model_info(trait)
res <- run_model_info(c(d, b), info, path)
