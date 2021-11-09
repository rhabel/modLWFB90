#' Creation of Climate Database
#'
#' This function creates a Project Climate Datafiles in the format required by \code{\link[LWFBrook90R]{run_LWFB90}}. \code{\link[LWFBrook90R]{run_multisite_LWFB90}} can read climate data from these files, that are no longer stored as a SQLite-DB, because parallel reading is not supportet
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param clim_dir directory, where climate files will be stored. will be created, if non existent
#' @param mindate first day of modelling time period as \code{Date}- object
#' @param maxdate last day of modelling time period as \code{Date}- object
#' @param points_at_once this functions processes a large amount of data in the memory. For performance purposes, the points in \code{df.ids} are split into batches of \code{points_at_once}. Default is \code{1000}, can be reduced if function crashes
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
                             points_at_once = 1000,

                             path_std = "R:/klima/whh/brook90_input/locations",
                             path_climdb = "R:/klima/whh/brook90_input/db/",
                             append = F) {

  # check if file exists, delete if T:
  if(append == F & dir.exists(clim_dir)){
    unlink(paste0(clim_dir, "*"))
  }
  if(!dir.exists(clim_dir)){
    dir.create(clim_dir)
  }


  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  minyear <- format(mindate, "%Y")
  maxyear <- format(maxdate, "%Y")
  needed_cols <- c("grhds", "rrds", "sddm", "tadm", "tadn", "tadx", "wsdm")
  # final_cols <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed", "vappres")
  colstoDelete <- c("sddm", "ewasser", "eeis", "year", "month", "day")

  # if more than points_at_once, split --------- ####
  # split into splits of 1000 due to RAM problems:
  if(nrow(df.ids) > points_at_once){

    begin_ids <- as.numeric(unlist(lapply(split(df.ids$ID,
                              ceiling(seq_along(df.ids$ID)/points_at_once)),
                        function(x) min(x))))
    end_ids <- as.numeric(unlist(lapply(split(df.ids$ID,
                            ceiling(seq_along(df.ids$ID)/points_at_once)),
                      function(x) max(x))))

    mapply(FUN = function(x, begin_id, end_id){

      # check which climate-ids ("id_standard") are needed:
      df.clim.ids <- fnc_relateCoords(df.ids = x[begin_id:end_id,],
                                      path_std = path_std)

      df.clim.ids <- df.clim.ids[c("id_standard", "tranche")]
      df.clim.ids <- unique(df.clim.ids)

      # if only one tranche in df.ids subset, apply parallel processing to number-of-cores batches
      if(length(unique(df.clim.ids$tranche)) == 1){

        n <- parallel::detectCores()
        nr <- nrow(df.clim.ids)
        df.clim.ids$batch <- rep(1:n, each=ceiling(nr/n), length.out=nr)

        cl <- parallel::makeCluster(parallel::detectCores())
        doParallel::registerDoParallel(cl)

        foreach::foreach(i = sort(unique(df.clim.ids$batch)),
                         .packages = c("RSQLite", "data.table", "dplyr"),
                         .export = c("path_climdb", "clim_dir",
                                     "minyear", "maxyear",
                                     "mindate", "maxdate",
                                     "needed_cols")) %dopar% {

                                       # preselection of ids
                                       ids_in_batch <- as.character(df.clim.ids[which(df.clim.ids$batch == i), "id_standard"])

                                       # make db connection
                                       con <- RSQLite::dbConnect(RSQLite::SQLite(),
                                                                 dbname = paste0(path_climdb, "climate_daily_obs_base_tr",
                                                                                 unique(df.clim.ids$tranche),".sqlite"))


                                       # get climate data
                                       clim.tmp <- dplyr::tbl(con, "climate_daily") %>%
                                         dplyr::filter(id %in% ids_in_batch) %>%
                                         dplyr::filter(year >= minyear & year <= maxyear) %>%
                                         dplyr::select(-grids) %>%
                                         dplyr::collect()

                                       RSQLite::dbDisconnect(con)

                                       # calculate additional meteorological properties that are needed LWFBrook90 the model run
                                       dt.clim.tmp <- data.table::as.data.table(clim.tmp)
                                       dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
                                       dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
                                       data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day",
                                                                           "globrad", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

                                       ### if we get complaints that our files get too large
                                       dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
                                       dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

                                       # create vappres
                                       dt.clim.tmp[, ewasser := 6.112*exp(17.62*tmean/(243.12+tmean))]
                                       dt.clim.tmp[, eeis := 6.112*exp(22.46*tmean/(272.62+tmean))]
                                       dt.clim.tmp[, vappres :=  round(ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1), 2)]

                                       # clean up
                                       dt.clim.tmp[, id_standard :=  as.character(id_standard)]
                                       dt.clim.tmp <- split(dt.clim.tmp, by = "id_standard")

                                       #### if we get complaints that our files get too large
                                       # dt.clim.tmp[ , (final_cols) := lapply(.SD, "/", 0.01), .SDcols = final_cols]


                                       lapply(dt.clim.tmp,
                                              function(x) saveRDS(x,
                                                                  file = paste0(clim_dir,
                                                                                x$id_standard[1],
                                                                                ".rds")))



                                     }
        parallel::stopCluster(cl)
      }else{
        cl <- parallel::makeCluster(parallel::detectCores())
        doParallel::registerDoParallel(cl)

        foreach::foreach(i = sort(unique(df.clim.ids$tranche)),
                         .packages = c("RSQLite", "data.table", "dplyr"),
                         .export = c("path_climdb", "clim_dir",
                                     "minyear", "maxyear",
                                     "mindate", "maxdate",
                                     "needed_cols")) %dopar% {

                                       # preselection of ids
                                       ids_in_tranche <- as.character(df.clim.ids[which(df.clim.ids$tranche == i), "id_standard"])

                                       # make db connection
                                       con <- RSQLite::dbConnect(RSQLite::SQLite(),
                                                                 dbname = paste0(path_climdb, "climate_daily_obs_base_tr",i,".sqlite"))


                                       # get climate data
                                       clim.tmp <- dplyr::tbl(con, "climate_daily") %>%
                                         dplyr::filter(id %in% ids_in_tranche) %>%
                                         dplyr::filter(year >= minyear & year <= maxyear) %>%
                                         dplyr::select(-grids) %>%
                                         dplyr::collect()

                                       RSQLite::dbDisconnect(con)

                                       # calculate additional meteorological properties that are needed LWFBrook90 the model run
                                       dt.clim.tmp <- data.table::as.data.table(clim.tmp)
                                       dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
                                       dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
                                       data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day",
                                                                           "globrad", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

                                       ### if we get complaints that our files get too large
                                       dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
                                       dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

                                       # create vappres
                                       dt.clim.tmp[, ewasser := 6.112*exp(17.62*tmean/(243.12+tmean))]
                                       dt.clim.tmp[, eeis := 6.112*exp(22.46*tmean/(272.62+tmean))]
                                       dt.clim.tmp[, vappres :=  round(ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1), 2)]

                                       # clean up
                                       dt.clim.tmp[, id_standard :=  as.character(id_standard)]
                                       dt.clim.tmp <- split(dt.clim.tmp, by = "id_standard")

                                       #### if we get complaints that our files get too large
                                       # dt.clim.tmp[ , (final_cols) := lapply(.SD, "/", 0.01), .SDcols = final_cols]


                                       lapply(dt.clim.tmp,
                                              function(x) saveRDS(x,
                                                                  file = paste0(clim_dir,
                                                                                x$id_standard[1],
                                                                                ".rds")))



                                     }
        parallel::stopCluster(cl)
      }

    },
           x = list(df.ids),
           begin_id = begin_ids,
           end_id = end_ids)
  }else{

    # check which climate-ids ("id_standard") are needed:
    df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                    path_std = path_std)

    df.clim.ids <- df.clim.ids[c("id_standard", "tranche")]
    df.clim.ids <- unique(df.clim.ids)

    # if only one tranche in df.ids subset, apply parallel processing to number-of-cores batches
    if(length(unique(df.clim.ids$tranche)) == 1){

      n <- parallel::detectCores()
      nr <- nrow(df.clim.ids)
      df.clim.ids$batch <- rep(1:n, each=ceiling(nr/n), length.out=nr)

      cl <- parallel::makeCluster(parallel::detectCores())
      doParallel::registerDoParallel(cl)

      foreach::foreach(i = sort(unique(df.clim.ids$batch)),
                       .packages = c("RSQLite", "data.table", "dplyr"),
                       .export = c("path_climdb", "clim_dir",
                                   "minyear", "maxyear",
                                   "mindate", "maxdate",
                                   "needed_cols")) %dopar% {

                                     # preselection of ids
                                     ids_in_batch <- as.character(df.clim.ids[which(df.clim.ids$batch == i), "id_standard"])

                                     # make db connection
                                     con <- RSQLite::dbConnect(RSQLite::SQLite(),
                                                               dbname = paste0(path_climdb, "climate_daily_obs_base_tr",
                                                                               unique(df.clim.ids$tranche),".sqlite"))


                                     # get climate data
                                     clim.tmp <- dplyr::tbl(con, "climate_daily") %>%
                                       dplyr::filter(id %in% ids_in_batch) %>%
                                       dplyr::filter(year >= minyear & year <= maxyear) %>%
                                       dplyr::select(-grids) %>%
                                       dplyr::collect()

                                     RSQLite::dbDisconnect(con)

                                     # calculate additional meteorological properties that are needed LWFBrook90 the model run
                                     dt.clim.tmp <- data.table::as.data.table(clim.tmp)
                                     dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
                                     dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
                                     data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day", "globrad", "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

                                     ### if we get complaints that our files get too large
                                     dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
                                     dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

                                     # create vappres
                                     dt.clim.tmp[, ewasser := 6.112*exp(17.62*tmean/(243.12+tmean))]
                                     dt.clim.tmp[, eeis := 6.112*exp(22.46*tmean/(272.62+tmean))]
                                     dt.clim.tmp[, vappres :=  round(ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1), 2)]

                                     # clean up
                                     dt.clim.tmp[, id_standard :=  as.character(id_standard)]
                                     dt.clim.tmp <- split(dt.clim.tmp, by = "id_standard")

                                     #### if we get complaints that our files get too large
                                     # dt.clim.tmp[ , (final_cols) := lapply(.SD, "/", 0.01), .SDcols = final_cols]


                                     lapply(dt.clim.tmp,
                                            function(x) saveRDS(x,
                                                                file = paste0(clim_dir,
                                                                              x$id_standard[1],
                                                                              ".rds")))



                                   }
      parallel::stopCluster(cl)
    }else{
      cl <- parallel::makeCluster(parallel::detectCores())
      doParallel::registerDoParallel(cl)

      foreach::foreach(i = sort(unique(df.clim.ids$tranche)),
                       .packages = c("RSQLite", "data.table", "dplyr"),
                       .export = c("path_climdb", "clim_dir",
                                   "minyear", "maxyear",
                                   "mindate", "maxdate",
                                   "needed_cols")) %dopar% {

                                     # preselection of ids
                                     ids_in_tranche <- as.character(df.clim.ids[which(df.clim.ids$tranche == i), "id_standard"])

                                     # make db connection
                                     con <- RSQLite::dbConnect(RSQLite::SQLite(),
                                                               dbname = paste0(path_climdb, "climate_daily_obs_base_tr",i,".sqlite"))

tic()
                                     # get climate data
                                     clim.tmp <- dplyr::tbl(con, "climate_daily") %>%
                                       dplyr::filter(id %in% ids_in_tranche) %>%
                                       dplyr::filter(year >= minyear & year <= maxyear) %>%
                                       dplyr::select(-grids) %>%
                                       dplyr::collect()
toc()
                                     RSQLite::dbDisconnect(con)

                                     # calculate additional meteorological properties that are needed LWFBrook90 the model run
                                     dt.clim.tmp <- data.table::as.data.table(clim.tmp)
                                     dt.clim.tmp <- data.table::setorder(dt.clim.tmp, id, year, month, day)
                                     dt.clim.tmp[ , (needed_cols) := lapply(.SD, "*", 0.01), .SDcols = needed_cols]
                                     data.table::setnames(dt.clim.tmp, c("id_standard", "year", "month", "day", "globrad",
                                                                         "prec", "sddm","tmean", "tmin", "tmax", "windspeed" ))

                                     ### if we get complaints that our files get too large
                                     dt.clim.tmp[, dates := as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")]
                                     dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

                                     # create vappres
                                     dt.clim.tmp[, ewasser := 6.112*exp(17.62*tmean/(243.12+tmean))]
                                     dt.clim.tmp[, eeis := 6.112*exp(22.46*tmean/(272.62+tmean))]
                                     dt.clim.tmp[, vappres :=  round(ifelse(tmean > 0, (ewasser-sddm)*0.1, (eeis-sddm)*0.1), 2)]

                                     # clean up
                                     dt.clim.tmp[, id_standard :=  as.character(id_standard)]
                                     dt.clim.tmp <- split(dt.clim.tmp, by = "id_standard")

                                     #### if we get complaints that our files get too large
                                     # dt.clim.tmp[ , (final_cols) := lapply(.SD, "/", 0.01), .SDcols = final_cols]


                                     lapply(dt.clim.tmp,
                                            function(x) saveRDS(x,
                                                                file = paste0(clim_dir,
                                                                              x$id_standard[1],
                                                                              ".rds")))



                                   }
      parallel::stopCluster(cl)
    }
  }

}

