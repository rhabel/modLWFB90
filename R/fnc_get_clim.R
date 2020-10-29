#' PTF application and humus layer creation
#'
#' This function takes the data frame of soil physics data and creates the hydraulic parameters. It further creates humus-layers using the MvG-parameters from Hammel&Kennel (2001)
#'
#' @param df.ids a data frame containing the following columns: \code{ID} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to. \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' @param mindate first day of modelling time period as \code{Date}- object
#' @param maxdate last day of modelling time period as \code{Date}- object
#' @param path_std path to standard locations directory
#' @param path_climdb path to climate-db directory
#'
#'
#' @return Returns a named list (names from \code{df.ids$ID}) of climate data
#' @export
#' @examples
#'
#' fnc_get_clim(df.ids = test.ids.bds, mindate = as.Date("2010-01-01"), maxdate = as.Date("2011-12-31"))

fnc_get_clim <- function(df.ids,
                         mindate = as.Date("2010-01-01"),
                         maxdate = as.Date("2011-12-31"),
                         path_std = "R:/klima/whh/brook90_input/locations",
                         path_climdb = "R:/klima/whh/brook90_input/db/") {

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")

  df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                  path_std = path_std)

  # initialise list
  ls.clim <- vector("list", length = nrow(df.ids))
  names(ls.clim) <- df.ids$ID

  for(tranche in sort(unique(df.clim.ids$tranche))){

    # preselection of ids
    ids_in_tranche <- as.character(df.clim.ids[which(df.clim.ids$tranche == tranche), "id_standard"])

    # make db connection
    con <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = paste0(path_climdb, "climate_daily_obs_base_tr",tranche,".sqlite"))

    # get climate data
    ls.clim.tmp <- RSQLite::dbGetQuery(con, paste0("SELECT * FROM climate_daily WHERE ( year >= ", minyear,
                                               " AND year <=", maxyear,
                                               ") AND (id = ", paste(ids_in_tranche, collapse = " OR id = "), ")")) %>%
      dplyr::arrange(id, year, month, day) %>%
      dplyr::mutate_at(vars(all_of(needed_cols)),
                       .funs = funs(. / 100)) %>%
      dplyr::rename(tmax = tadx,
                    tmin = tadn,
                    tmean = tadm,
                    wind = wsdm,
                    prec = rrds,
                    globrad = grhds,
                    id_standard = id) %>%
      dplyr::mutate(dates = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d"),
                    ewasser = 6.11*10^(7.5*tmean/(273.5+tmean)),
                    eeis = 6.11*10^(9.5*tmean/(265.5+tmean)),
                    vappres = case_when(tmean > 0 ~ (ewasser-sddm)*0.1, # kPa
                                        T ~ (eeis-sddm)*0.1),
                    id_standard = as.character(id_standard)) %>%
      dplyr::left_join(df.clim.ids[c("ID", "id_standard")], by = "id_standard") %>%
      dplyr::select(ID, id_standard, dates, year, month, day, globrad, prec, tmean, tmin, tmax, wind, vappres) %>%
      dplyr::filter(dates >= mindate & dates <= maxdate) %>%

      dplyr::group_split(id_standard)
    dbDisconnect(con)

    # names correct...
    names(ls.clim.tmp) <- unlist(lapply(ls.clim.tmp, function(x) unique(x$ID)))


    ls.clim[match(names(ls.clim.tmp), df.ids$ID)] <- ls.clim.tmp


  }



  # return resulting list
  return(ls.clim)
}
