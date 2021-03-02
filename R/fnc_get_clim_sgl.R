#' Reading data directly from climate db
#'
#' This function returns climate data from the main FVA-Climate Databases in the format required by \code{\link[LWFBrook90R]{run_LWFB90}}. \code{\link[LWFBrook90R]{run_multisite_LWFB90}} can read climate data directly via \code{climate} and \code{climate_args} without loading data into the session's memory. \cr\cr See example for how to set \code{climate_args} correctly.
#'
#' @param id ID_custom of the point
#' @param easting UTM32632 easting of respective point
#' @param northing UTM32632 northing of respective point
#' @param mindate first day of modelling time period as \code{Date}- object
#' @param maxdate last day of modelling time period as \code{Date}- object
#' @param path_std path to standard locations directory
#' @param path_climdb path to climate-db directory
#'
#' @import data.table
#'
#' @return creates climate data on-the-fly for \code{\link[LWFBrook90R]{run_multiside_LWFB90}} by returning a correct clim-df
#' @example inst/examples/fnc_get_clim_sgl_ex.R
#'
#' @export

fnc_get_clim_sgl <- function(id,
                             easting,
                             northing,
                             mindate = as.Date("2010-01-01"),
                             maxdate = as.Date("2011-10-31"),
                             path_std = "R:/klima/whh/brook90_input/locations",
                             path_climdb = "R:/klima/whh/brook90_input/db/") {



  # IDs okay? ---------- ####
  # sort dfs according to IDs
  df.id <- data.frame("ID_custom" = id,
                         "easting" = easting,
                         "northing" = northing)

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")

  # get climate id
  df.clim.ids <- fnc_relateCoords(df.ids = df.id,
                                  path_std = path_std)



  # make db connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(),
                            dbname = paste0(path_climdb, "climate_daily_obs_base_tr",df.clim.ids$tranche,".sqlite"))

  # get climate data
  clim.tmp <- RSQLite::dbGetQuery(con, paste0("SELECT * FROM climate_daily WHERE ( year >= ", minyear,
                                              " AND year <=", maxyear,
                                              " ) AND id = ", df.clim.ids$id_standard))

  RSQLite::dbDisconnect(con)

  # calculate additional meteorological properties that are needed LWFBrook90 the model run
  dt.clim.tmp <- data.table::as.data.table(clim.tmp)

  # restore units
  dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]

  # rename
  data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day", "globrad", "grids", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

  # date- creation and selection
  dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
  dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]
  data.table::setorder(dt.clim.tmp, dates)

  # vappres calculation
  dt.clim.tmp[, ewasser := 6.11*10^(7.5*tmean/(273.5+tmean))]
  dt.clim.tmp[, eeis := 6.11*10^(9.5*tmean/(265.5+tmean))]
  dt.clim.tmp[, vappres :=  ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1)]

  # add IDs and select
  dt.clim.tmp[, id_standard :=  as.character(id_standard)]
  dt.clim.tmp[, ID_custom :=  as.character(df.clim.ids$ID_custom)]
  cols <- c("ID_custom", "id_standard", "dates", "globrad", "prec", "tmean", "tmax", "tmin", "windspeed", "vappres")
  dt.clim.tmp <- as.data.frame(dt.clim.tmp[,..cols])

  return(dt.clim.tmp)
}

# clim_args <-
#   lapply(split(df.ids, seq(nrow(df.ids))),
#          function(x) list(id = as.character(x$ID_custom),
#                           easting = x$easting,
#                           northing = x$northing,
#                           mindate = as.Date("2009-01-01"),
#                           maxdate = as.Date("2010-12-31")))

