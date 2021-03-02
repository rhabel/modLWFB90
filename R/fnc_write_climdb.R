#' Creation of Climate Database
#'
#' This function creates a Project Climate Datafiles in the format required by \code{\link[LWFBrook90R]{run_LWFB90}}. \code{\link[LWFBrook90R]{run_multisite_LWFB90}} can read climate data from these files, that are no longer stored as a SQLite-DB, because parallel reading is not supportet
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param mindate first day of modelling time period as \code{Date}- object
#' @param maxdate last day of modelling time period as \code{Date}- object
#'
#' @param path_std path to standard locations directory
#' @param path_climdb path to climate-db directory
#' @param append shall the data already in the climate-directory be deleted first. Directory is cleared if \code{FALSE}
#'
#' @import data.table parallel doParallel foreach
#'
#' @return creates as SQLite-Database with one climate-df for each ID ind \code{df.ids$ID_custom})
#' @example inst/examples/fnc_climdb_ex.R
#'
#' @export

fnc_write_climdb <- function(df.ids,
                             clim_dir,

                             mindate = as.Date("2010-01-01"),
                             maxdate = as.Date("2011-12-31"),

                             path_std = "R:/klima/whh/brook90_input/locations",
                             path_climdb = "R:/klima/whh/brook90_input/db/",
                             append = F) {

  # check if file exists, delete if T:
  if(append == F & dir.exists(clim_dir)){
    unlink(paste0(clim_dir, "*"))
  }


  # IDs okay? ---------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")
  # final_cols <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed", "vappres")
  colstoDelete <- c("grids", "sddm", "ewasser", "eeis", "year", "month", "day")

  # check which climate-ids ("id_standard") are needed:
  df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                  path_std = path_std)

  df.clim.ids <- df.clim.ids[c("id_standard", "tranche")]
  df.clim.ids <- unique(df.clim.ids)

  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)

  foreach::foreach(i = sort(unique(df.clim.ids$tranche)),
                   .packages = c("RSQLite", "data.table")) %dopar% {

    # preselection of ids
    ids_in_tranche <- as.character(df.clim.ids[which(df.clim.ids$tranche == i), "id_standard"])

    # make db connection
    con <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = paste0(path_climdb, "climate_daily_obs_base_tr",i,".sqlite"))

    # get climate data
    clim.tmp <- RSQLite::dbGetQuery(con, paste0("SELECT * FROM climate_daily WHERE ( year >= ", minyear,
                                                " AND year <=", maxyear,
                                                ") AND (id = ", paste(ids_in_tranche, collapse = " OR id = "), ")"))

    RSQLite::dbDisconnect(con)


    # calculate additional meteorological properties that are needed LWFBrook90 the model run
    dt.clim.tmp <- data.table::as.data.table(clim.tmp)
    dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
    dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
    data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day", "globrad", "grids", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

    ### if we get complaints that our files get too large
    dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
    dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

    # create vappres
    dt.clim.tmp[, ewasser := 6.11*10^(7.5*tmean/(273.5+tmean))]
    dt.clim.tmp[, eeis := 6.11*10^(9.5*tmean/(265.5+tmean))]
    dt.clim.tmp[, vappres :=  round(ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1), 2)]

    # clean up
    dt.clim.tmp[, id_standard :=  as.character(id_standard)]
    dt.clim.tmp[, (colstoDelete) := NULL]
    dt.clim.tmp <- split(dt.clim.tmp, by = "id_standard")

    #### if we get complaints that our files get too large
    # dt.clim.tmp[ , (final_cols) := lapply(.SD, "/", 0.01), .SDcols = final_cols]


    lapply(dt.clim.tmp,
           function(dt.clim.tmp) save(dt.clim.tmp,
                                  file = paste0(clim_dir,
                                                unique(dt.clim.tmp$id_standard),
                                                ".RData")))


#     # write to db
#     con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbout_name)
#
#     RSQLite::dbWriteTable(con,
#                           "clim",
#                           dt.clim.tmp,
#                           append=T, overwrite = F, row.names=F)
#
#
#     RSQLite::dbDisconnect(con)

  }
  parallel::stopCluster(cl)
}

