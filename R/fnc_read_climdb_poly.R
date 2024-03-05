#' Reading from Climate Database when input is a Polygon Layer
#'
#' This function reads in the Project Climate Data files in \code{"R:/klima/whh/brook90_input/rds/"} plus the respective tranche, and processes the data correctly, so that it can be used via the function parameters \code{climate} and \code{climate_args} in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}. Function arguments must be passed to \code{\link[LWFBrook90R]{run_multisite_LWFB90}} as a list of lists, which is created in \code{\link{fnc_get_climate_args_poly}} See example on how to do this.
#'
#' @param ID_custom ID-name of point in project climate data base
#' @param clim_dir path to project climate data files
#' @param weights vector of weights, which are being calculated from the percentage of climate-cell area covered by the respective polygon
#' @param mindate R-date-object, optional. Set if only a selection of the data (1961-2020) in the climate-database shall be retrieved.
#' @param maxdate R-date-object, optional, same as mindate. Must be provided with mindate if sub-selection on date is desired.
#'
#' @return returns climate data in correct format
#' @example inst/examples/fnc_poly_ex.R
#'
#' @export

fnc_read_climdb_poly <- function(IDs,

                                 clim_dir,
                                 weights,

                                 mindate = NA,
                                 maxdate = NA) {

  # create list of climate data frames
  climlist <- lapply(as.list(clim_dir), readRDS)
  # create weights-columns
  climlist <- mapply(FUN = function(x, weights){
                       x$weight <- weights
                       return(x)
                     },
                     x = climlist,
                     weights = weights,
                     SIMPLIFY = F)

  # convert to data.table, create date-column
  climlist <- lapply(climlist, function(x){
    x <- as.data.table(x)
    x[,"dates" := as.Date(paste(year, month, day, sep = "-"))]
    x[,c("sddm", "year", "month", "day") := NULL]

    # filter dates
    if(!is.na(mindate)){
      x <- x[dates>= mindate & dates <= maxdate]
    }

    return(x)
    }
  )

  dt.clim.tmp <- rbindlist(climlist)
  dt.clim.tmp <- dt.clim.tmp[, by = dates,
                             lapply(.SD, stats::weighted.mean, w = weight),
                             .SDcols = c("grhds", "rrds", "tadx", "tadm", "tadn", "wsdm", "vappres")]

  # add ID_custom and dates, remove sddm
  dt.clim.tmp[,"ID_custom" := IDs]


  # names and units
  cols_0_1 <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed")
  setnames(dt.clim.tmp,
           old = c("grhds", "rrds", "tadx", "tadm", "tadn", "wsdm"),
           new = c("globrad", "prec", "tmax", "tmean", "tmin", "windspeed"))
  dt.clim.tmp[ , (cols_0_1) := lapply(.SD, function(x){round(x*0.1,1)}), .SDcols = cols_0_1]
  dt.clim.tmp[ , vappres := round(vappres * 0.01, 2)]

  return(dt.clim.tmp)
}

