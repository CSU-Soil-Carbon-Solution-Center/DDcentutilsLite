#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
do.call(yourpkg::daycent_main_cli, list(args))
