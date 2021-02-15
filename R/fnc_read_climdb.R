#' Reading from Climate Database
#'
#' This function reads in the Project Climate Database written by \code{\link[LWFBrook90R]{fnc_write_climdb}} and processes the data correctly, so that it can be used via the function parameters \code{climate} and \code{climate_args} in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}. Function arguments must be passed to \code{\link[LWFBrook90R]{run_multisite_LWFB90}} as list of lists. Upper level containing the ID_custom names of the points to be modelled, the lower level containing the respective ID_custom name as well as the path to the project climate data base
#'
#' @param ID_custom ID-name of point in project climate data base
#' @param path_climdb path to project climate data base
#'
#' @return returns climate data in correct format
#' @examples inst/examples/fnc_get_soil_ex.R
#'
#' @export

fnc_read_climdb <- function(IDs,
                            path_climdb) {

  db <- DBI::dbConnect(drv = RSQLite::SQLite(),
                  dbname = path_climdb)

  clim <- dplyr::tbl(db, "clim") %>%
    dplyr::select(dplyr::everything()) %>%
    dplyr::filter(ID_custom == IDs) %>%
    dplyr::collect() %>%
    dplyr::mutate(month = formatC(month, width = 2,  flag = "0"),
                  day = formatC(day, width = 2,  flag = "0"),
                  dates = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")) %>%
    dplyr::select(ID, ID_custom, dates, globrad, prec, tmean, tmin, tmax, windspeed, vappres)

  DBI::dbDisconnect(db)

  return(clim)
}

