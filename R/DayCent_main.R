#' Run DayCent for a site/scenario
#'
#' @param site Name of the site folder under 'sites/'.
#' @param run Scenario or run identifier.
#' @param config_file Path to config file defining dc_exe and dc_path100.
#' @param run_eq Logical; run equilibrium + base when TRUE (default).
#' @param strict Logical; fail on warnings when TRUE.
#'
#' @import logger
#'
#' @return Invisibly returns DayCent log or output object.
#'
#' @export
daycent_main <- function(site, run, config_file, run_eq = TRUE, strict = FALSE) {
  if (isTRUE(strict)) {
    op <- options(warn = 2); on.exit(options(op), add = TRUE)
  }

  stopf <- function(...) stop(sprintf(...), call. = FALSE)
  must_exist_dir  <- function(p) if (!dir.exists(p))  stopf("Missing directory: %s", p)
  must_exist_file <- function(p) if (!file.exists(p)) stopf("Missing file: %s", p)

  site_dir <- file.path("sites", site)
  must_exist_dir(site_dir)

  config_file <- normalizePath(config_file, mustWork = FALSE)
  must_exist_file(config_file)

  cfg <- new.env(parent = emptyenv())
  sys.source(config_file, envir = cfg)
  if (!exists("dc_exe", envir = cfg))     stopf("dc_exe not defined in config")
  if (!exists("dc_path100", envir = cfg)) stopf("dc_path100 not defined in config")

  dc_exe     <- cfg$dc_exe
  dc_path100 <- cfg$dc_path100
  must_exist_file(dc_exe)
  must_exist_dir(dc_path100)

  log_file <- file.path(site_dir, paste0(site, "_", run, ".log"))
  logger::log_appender(logger::appender_tee(log_file))
  logger::log_layout(logger::layout_glue_colors)
  logger::log_info("Initialized run: site={site}, run={run}, config={config_file}, run_eq={run_eq}")

  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd(site_dir)
  logger::log_info("Now in site_dir: {getwd()}")

  run_safe <- function(expr, msg) {
    tryCatch(expr, error = function(e) {
      logger::log_error("{msg}: {e$message}")
      stopf("%s: %s", msg, e$message)
    })
  }

  out <- if (isTRUE(run_eq)) {
    logger::log_info("Running equilibrium + base")
    run_safe(
      DDcentutilsLite::DayCentRunSite(site, run, run_base = TRUE, run_eq = TRUE,
                     dc_exe_in = dc_exe, dc_path100_in = dc_path100),
      "DayCentRunSite failed"
    )
  } else {
    logger::log_info("Running single pass (no equilibrium)")
    run_safe(
      DDcentutilsLite::DayCentRunSite_single_run(site, run,
                                dc_exe_in = dc_exe, dc_path100_in = dc_path100),
      "DayCentRunSite_single_run failed"
    )
  }

  if (is.null(out) || NROW(out) == 0) stopf("Run produced no log output.")
  logger::log_info("Run completed successfully.")
  cat("Done!\n")

  invisible(out)
}

#' Command-line interface for DayCent main runner
#' @param args character vector, usually commandArgs(trailingOnly = TRUE)
#' @export
daycent_main_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  print_help <- function(script = "daycent-main") {
    cat(sprintf("
Usage:
  Rscript -e 'yourpkg::daycent_main_cli()' --args <site> <run> <config_file> [run_eq]

Arguments:
  site         Folder under 'sites/' (e.g. 'canola_CO')
  run          Scenario/run name (e.g. 'base')
  config_file  Path to R config defining dc_exe and dc_path100
  run_eq       TRUE/FALSE, 1/0, yes/no  [default: TRUE]

Examples:
  Rscript -e 'DDcentutilsLite::daycent_main_cli()' --args wooster cc_ct ./config/{config}.R TRUE
"))
  }

  parse_bool <- function(x) {
    if (missing(x) || is.null(x)) return(TRUE)
    x <- tolower(as.character(x))
    if (x %in% c("1","true","t","yes","y"))  return(TRUE)
    if (x %in% c("0","false","f","no","n"))  return(FALSE)
    stop(sprintf("Invalid boolean: %s", x), call. = FALSE)
  }

  if (!length(args) || args[1] %in% c("-h","--help")) return(print_help())
  if (length(args) < 3) { cat("Not enough arguments.\n\n"); return(print_help()) }

  site   <- args[1]
  run    <- args[2]
  config <- args[3]
  run_eq <- if (length(args) >= 4) parse_bool(args[4]) else TRUE

  daycent_main(site, run, config, run_eq)
}
