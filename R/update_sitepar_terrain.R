#' @title Update Sitepar Terrain Values (elevation, slope, aspect)
#'
#' @description Replaces terrain lines in a sitepar file with values from a terrain list.
#'
#' @param sitepar_path Path to the input file.
#' @param terrain_list A list with elements `elevation`, `slope`, and `aspect`.
#' @param output_path Optional path to save the updated file. If NULL, returns updated lines.
#'
#' @return Updated lines invisibly or writes to a file.
#' 
#' @export
update_sitepar_terrain <- function(sitepar_path, terrain_list, output_path = NULL) {
  lines <- readLines(sitepar_path)
  
  pattern_elev  <- "^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*/\\s*elevation"
  pattern_slope <- "^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*/\\s*slope"
  pattern_aspect <- "^\\s*[-+]?[0-9]*\\.?[0-9]+\\s*/\\s*aspect"
  
  elev_line   <- grep(pattern_elev, lines)
  slope_line  <- grep(pattern_slope, lines)
  aspect_line <- grep(pattern_aspect, lines)
  
  if (length(elev_line) != 1 || length(slope_line) != 1 || length(aspect_line) != 1) {
    stop("Could not uniquely identify elevation, slope, or aspect lines.")
  }
  
  elev_comment   <- sub(".*(/.*)", "\\1", lines[elev_line])
  slope_comment  <- sub(".*(/.*)", "\\1", lines[slope_line])
  aspect_comment <- sub(".*(/.*)", "\\1", lines[aspect_line])
  
  lines[elev_line]   <- sprintf("%.2f      %s", terrain_list$elevation, elev_comment)
  lines[slope_line]  <- sprintf("%.2f      %s", terrain_list$slope, slope_comment)
  lines[aspect_line] <- sprintf("%.2f      %s", terrain_list$aspect, aspect_comment)
  
  if (!is.null(output_path)) {
    writeLines(lines, output_path)
  } else {
    return(lines)
  }
}