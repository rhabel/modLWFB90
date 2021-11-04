#' Reading from Climate Database
#'
#' This function reads in the Project Climate Data files written by \code{\link{fnc_write_climdb}} and processes the data correctly, so that it can be used via the function parameters \code{climate} and \code{climate_args} in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}. Function arguments must be passed to \code{\link[LWFBrook90R]{run_multisite_LWFB90}} as list of lists. Upper level containing the ID_custom names of the points to be modelled, the lower level containing the respective ID_custom name as well as the path to the project climate data base
#'
#' @param ID_custom ID-name of point in project climate data base
#' @param clim_dir path to project climate data file directory
#' @param id_standard climate-cell where point is located, use \code{\link{fnc_relateCoords}} to create this information, see example
#' @param mindate R-date-object, optional, if only a selection of the data in the climate-database shall be retrieved, can be used to improve computing time
#' @param maxdate R-date-object, optional, same as mindate. Must be provided with mindate if sub-selection on date is desired.
#'
#' @return returns climate data in correct format
#' @example inst/examples/fnc_climdb_ex.R
#'
#' @export

fnc_read_climdb <- function(IDs,
                            clim_dir,
                            id_standard,

                            mindate = NA,
                            maxdate = NA) {

  df.id <- data.frame("ID_custom" = IDs,
                      "id_standard" = id_standard)


  dt.clim.tmp <- readRDS(paste0(clim_dir, df.id$id_standard, ".rds"))
  dt.clim.tmp[,"ID_custom" := IDs]

  #### if we get complaints that our files get too large, only integers get stored in .RData with...
  # final_cols <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed", "vappres")
  # dt.clim.tmp[ , (final_cols) := lapply(.SD, "*", 0.01), .SDcols = final_cols]

  if(!is.na(mindate)){
    dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]
  }

  return(dt.clim.tmp)
}

