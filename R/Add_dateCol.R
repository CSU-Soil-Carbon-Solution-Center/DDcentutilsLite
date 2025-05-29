#' @title Convert time and day of the year columns to Date
#'
#' @description This function converts the 'time' and 'dayofyr' columns of a dataframe into a proper 'Date' column.
#'
#' @param df data frame. A dataframe containing 'time' and 'dayofyr' columns.
#' @param time_col character. The name of the 'time' column (default is 'time').
#' @param dayofyr_col character. The name of the 'dayofyr' column (default is 'dayofyr').
#' @param date_col character. The name of the new 'Date' column to be created (default is 'date').
#'
#' @return The input dataframe with an added 'Date' column.
#'
#' @export
Add_dateCol <- function(df, time_col = "time", dayofyr_col = "dayofyr", date_col = "date") {
  df[[date_col]] <- as.Date(paste0(floor(df[[time_col]]), "/", df[[dayofyr_col]]), format = "%Y/%j")
  return(df)
}
