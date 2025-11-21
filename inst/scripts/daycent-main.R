#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
do.call(DDcentutilsLite::daycent_main_cli, list(args))
