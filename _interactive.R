# script for quickly loading

source("_packages.R")
source("R/functions.R")
source("R/utils.R")

ts <- as.list(targets::tar_manifest(fields = name)[["name"]])
names(ts) <- ts

cnf <- config::get(config = "default")
