# script for quickly loading

source("_targets_packages.R")
source("R/utils.R")
source("R/functions.R")

options(scipen = 9)

ts <- as.list(targets::tar_manifest(fields = name)[["name"]])
names(ts) <- ts

cnf <- config::get(config = "default")
