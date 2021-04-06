#' Creation of Climate Data
#'
#' This function creates Climate Data in the format required by \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}}.
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
#'
#' @import data.table
#' @return Returns a named list (names from \code{df.ids$ID_custom}) of climate data
#' @examples
#' fnc_get_clim(df.ids = test.ids.bds, mindate = as.Date("2010-01-01"), maxdate = as.Date("2011-12-31"))
#'
#' @export

fnc_get_clim <- function(df.ids,
                         mindate = as.Date("2010-01-01"),
                         maxdate = as.Date("2011-12-31"),
                         path_std = "R:/klima/whh/brook90_input/locations",
                         path_climdb = "R:/klima/whh/brook90_input/db/") {

  # IDs okay? ---------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")

  df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                  path_std = path_std)

  # initialise list
  ls.clim <- vector("list", length = nrow(df.ids))
  names(ls.clim) <- df.ids$ID_custom

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

    ls.clim.tmp <- data.table::as.data.table(clim.tmp)
    ls.clim.tmp <- data.table::setorder(ls.clim.tmp, id, year, month, day)
    ls.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
    data.table::setnames(ls.clim.tmp, c("id_standard", "year", "month", "day", "globrad", "grids", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" )) # constr_corg umbenennen
    ls.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
    ls.clim.tmp[, ewasser := 6.112*exp(17.62*tmean/(243.12+tmean))]
    ls.clim.tmp[, eeis := 6.112*exp(22.46*tmean/(272.62+tmean))]
    ls.clim.tmp[, vappres :=  ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1)]
    ls.clim.tmp[, id_standard :=  as.character(id_standard)]

    # join
    df.clim.ids.j <- data.table::as.data.table(df.clim.ids[c("ID", "ID_custom", "id_standard")])
    data.table::setkey(df.clim.ids.j, id_standard)
    data.table::setkey(ls.clim.tmp, id_standard)

    ls.clim.tmp <- merge(x = ls.clim.tmp, y = df.clim.ids.j, by = "id_standard", all.x = TRUE, allow.cartesian = T)
    # ls.clim.tmp <- ls.clim.tmp[df.clim.ids.j]

    data.table::setorder(ls.clim.tmp, ID_custom, year, month, day)
    #
    ls.clim.tmp[, ID_custom := factor(ID_custom, levels = unique(ID_custom))]

    ls.clim.tmp <- ls.clim.tmp[,.(ID, ID_custom, id_standard, dates, year, month, day, globrad, prec, tmean, tmin, tmax, windspeed, vappres)]
    ls.clim.tmp <- ls.clim.tmp[dates>= mindate & dates <= maxdate]

    ls.clim.tmp <- split(ls.clim.tmp, ls.clim.tmp$ID_custom)

    # is.data.table(ls.clim.tmp)

    # names ang assigning correct...
    names(ls.clim.tmp) <- unlist(lapply(ls.clim.tmp, function(x) unique(x$ID_custom)))

    ls.clim[match(names(ls.clim.tmp), df.ids$ID_custom)] <- ls.clim.tmp


  }

  ls.clim <- lapply(ls.clim, as.data.frame, stringsAsFactors = F)
  # return resulting list
  return(ls.clim)
}
