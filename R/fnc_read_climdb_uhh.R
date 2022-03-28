#' Reading from Climate Database
#'
#' This function reads in the Project Climate Data files from the UHH-Climate data and processes the data correctly, so that it can be used via the function parameters \code{climate} and \code{climate_args} in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}. Function arguments must be passed to \code{\link[LWFBrook90R]{run_multisite_LWFB90}} as a list of lists, which is created here. See example on how to do this.
#'
#' @param ID_custom ID-name of point in project climate data base
#' @param clim_dir path to project climate data file directory
#' @param id_standard climate-cell where point is located, use \code{\link{fnc_relateCoords}} to create this information, see example
#' @param mindate R-date-object, optional. Set if only a selection of the data (1961-2018) in the climate-database shall be retrieved.
#' @param maxdate R-date-object, optional, same as mindate. Must be provided with mindate if sub-selection on date is desired.
#'
#' @return returns climate data in correct format
#' @example inst/examples/fnc_climdb_ex.R
#'
#' @export

fnc_read_climdb_uhh <- function(IDs,
                                clim_dir,
                                id_standard,

                                mindate = NA,
                                maxdate = NA) {

  # load and convert to data.table
  dt.clim.tmp <- readRDS(paste0(clim_dir, id_standard, ".rds"))
  dt.clim.tmp <- as.data.table(dt.clim.tmp)

  # add ID_custom and dates, remove sddm
  dt.clim.tmp[,"ID_custom" := IDs]
  dt.clim.tmp[,"dates" := as.Date(paste(stringr::str_sub(date, 1,4),
                                        stringr::str_sub(date, 5,6),
                                        stringr::str_sub(date, 7,8),
                                        sep = "-"))]
  setorder(dt.clim.tmp, dates)
  dt.clim.tmp[,c("date") := NULL]

  # filter dates
  if(!is.na(mindate)){
    dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]
  }

  # names and units
  cols_0_1 <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed")
  setnames(dt.clim.tmp,
           old = c("sgz_cor", "prz_cor", "tmx_cor", "tav_cor", "tmn_cor", "wsp_cor", "vap_cor"),
           new = c("globrad", "prec", "tmax", "tmean", "tmin", "windspeed", "vappres"))
  dt.clim.tmp[ , (cols_0_1) := lapply(.SD, "*", 0.1), .SDcols = cols_0_1]
  dt.clim.tmp[ , vappres := vappres  * 0.01]

  return(dt.clim.tmp)
}

