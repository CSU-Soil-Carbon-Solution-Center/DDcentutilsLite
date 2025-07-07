#' @title Download NASA Power weather data for DayCent run
#'
#' @description  This function retrieves the NASA Power weather data of a specific site based on its latitude and longitude
#' using the `nasapower` package. The output includes weather variables necessary for a DayCent run: day of the month,
#' month, maximum and minimum temperature (in degrees Celsius), daily precipitation (cm), and shortwave radiation (W/m2).
#'
#' @param raw_data_path character. Specifies the path where the Daymet files will be saved. Default saves files in temporary folder.
#' @param site character. The name or identifier for the site.
#' @param lat numeric. Latitude of the site in decimal degrees.
#' @param lon numeric. Longitude of the site in decimal degrees.
#' @param start numeric. Specifies the start year to download weather data.
#' @param end numeric. Specifies the end year to download weather data.
#'
#' @return A data frame with all dates from the start to the end year and data for weather variables necessary for a DayCent run.
#'
#' @details
#' The function downloads NASA Power weather data based on a user-specified time interval using data from the Agroclimatology Archive.
#' The returned data frame includes the following order necessary for a DayCent run:
#' \itemize{
#' \item day, corresponds to the day of the month.
#' \item month, corresponds to the month of the year.
#' \item year, correponds to the year.
#' \item yday, corresponds to the day of the year.
#' \item tmax_C, maximum temperature, in C.
#' \item tmin_C, minimum temperature, in C.
#' \item prcp_cm_day, daily precipitation, in cm.
#' }
#' These seven variables are required in all weather files regardless of the DayCent version.
#' Nonetheless, the returned data frame also includes:
#' \itemize{
#' \item srad_Wm2, shortwave radiation, in W/m2.
#' \item RH2M, relative humidity, in %.
#' \item WS2M, wind speed at 2 meters, in mph.
#' }
#'
#' @examples
#' # Example usage
#' lat <- 40.58
#' lon <- -105.08
#' site <- "Sample Site"
#' weather <- getNASAPowerData(raw_data_path = NULL, site = site,
#'                             lat = lat, lon = lon, start = 2000, end = 2020)
#'
#' @import nasapower
#' @import dplyr
#'
#' @export
getNASAPowerData <- function(raw_data_path = NULL,
                             site = site, lat = lat, lon = lon,
                             start = start, end = end){

  weather_data <- get_power(
    community = "ag",
    pars = c("T2M_MIN", "T2M_MAX", "PRECTOTCORR", "RH2M", "ALLSKY_SFC_SW_DWN", "WS2M" ),
    # need all the parameters here: Tmin (C), Tmax (C), Prcip (mm/day), RHm (%), ALLSKY_SFC_SW_DWN (W/m²), wind (m/s),
    temporal_api = "daily",
    lonlat = c(lon,lat),
    dates = c(paste0(start, "-01-01"), paste0(end, "-12-31")))

  # Updating units and aligning with daycent variable order
  weather_data_DayCent <- weather_data %>% mutate(day = day(YYYYMMDD),
                                                  month = month(YYYYMMDD),
                                                  yday = DOY,
                                                  year = YEAR,
                                                  tmax_C = T2M_MAX,
                                                  tmin_C = T2M_MIN,
                                                  prcp_cm_day = PRECTOTCORR/10,
                                                  RH2M = RH2M,
                                                  srad_Wm2 = ALLSKY_SFC_SW_DWN*41.67, #converting from kwh m2 to W m2
                                                  WS2M = WS2M*2.236) %>% # from ms to mph
    dplyr::select(day, month, year, yday, tmax_C, tmin_C, prcp_cm_day, srad_Wm2, RH2M, WS2M)
}

