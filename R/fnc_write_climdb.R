#' Creation of Climate Database
#'
#' This function creates a Project Climate Database in the format required by \code{\link[LWFBrook90R]{run_LWFB90}}. \code{\link[LWFBrook90R]{run_multisite_LWFB90}} can read climate data from this newly created database providing the advantage of lesser RAM consumption.
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param mindate first day of modelling time period as \code{Date}- object
#' @param maxdate last day of modelling time period as \code{Date}- object
#' @param path_std path to standard locations directory
#' @param path_climdb path to climate-db directory
#' @param dbout_name full path and file name for output-climate-db directory
#'
#' @import data.table
#'
#' @return creates as SQLite-Database with one climate-df for each ID ind \code{df.ids$ID_custom})
#' @example inst/examples/fnc_climdb_ex.R
#'
#' @export

fnc_write_climdb <- function(df.ids,
                         mindate = as.Date("2010-01-01"),
                         maxdate = as.Date("2011-12-31"),
                         path_std = "R:/klima/whh/brook90_input/locations",
                         path_climdb = "R:/klima/whh/brook90_input/db/",
                         dbout_name,
                         append_to_db = F) {

  # check if file exists, delete if T:

  if(append_to_db == F & file.exists(dbout_name)){
    unlink(dbout_name)
  }


  # IDs okay? ---------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")

  df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                  path_std = path_std)

  for(tranche in sort(unique(df.clim.ids$tranche))){

    # preselection of ids
    ids_in_tranche <- as.character(df.clim.ids[which(df.clim.ids$tranche == tranche), "id_standard"])

    # make db connection
    con <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = paste0(path_climdb, "climate_daily_obs_base_tr",tranche,".sqlite"))

    # get climate data
    clim.tmp <- RSQLite::dbGetQuery(con, paste0("SELECT * FROM climate_daily WHERE ( year >= ", minyear,
                                                " AND year <=", maxyear,
                                                ") AND (id = ", paste(ids_in_tranche, collapse = " OR id = "), ")"))

    RSQLite::dbDisconnect(con)

    # calculate additional meteorological properties that are needed LWFBrook90 the model run
    dt.clim.tmp <- data.table::as.data.table(clim.tmp)
    dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
    dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
    data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day", "globrad", "grids", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" )) # constr_corg umbenennen
    dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
    dt.clim.tmp[, ewasser := 6.11*10^(7.5*tmean/(273.5+tmean))]
    dt.clim.tmp[, eeis := 6.11*10^(9.5*tmean/(265.5+tmean))]
    dt.clim.tmp[, vappres :=  ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1)]
    dt.clim.tmp[, id_standard :=  as.character(id_standard)]

    # join
    df.clim.ids.j <- data.table::as.data.table(df.clim.ids[c("ID", "ID_custom", "id_standard")])
    data.table::setkey(df.clim.ids.j, id_standard)
    data.table::setkey(dt.clim.tmp, id_standard)

    dt.clim.tmp <- merge(x = dt.clim.tmp, y = df.clim.ids.j, by = "id_standard", all.x = TRUE, allow.cartesian = T)

    data.table::setorder(dt.clim.tmp, ID_custom, year, month, day)
    #
    dt.clim.tmp[, ID_custom := factor(ID_custom, levels = unique(ID_custom))]

    dt.clim.tmp <- dt.clim.tmp[,.(ID, ID_custom, id_standard, dates, year, month, day, globrad, prec, tmean, tmin, tmax, windspeed, vappres)]
    dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]
    dt.clim.tmp[, dates:= NULL]


    # write to db
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbout_name)

    RSQLite::dbWriteTable(con,
                          "clim",
                          dt.clim.tmp,
                          append=T, overwrite = F, row.names=F)


    RSQLite::dbDisconnect(con)

  }
}

