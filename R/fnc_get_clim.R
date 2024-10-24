#' Creation of Climate Data
#'
#' This function creates Climate Data in the format required by \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}}. Output is a list for direct use as input, not via \code{clim_args} and \code{args_function}.
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param mindate first day of modelling time period as \code{Date}- object. Optional, default is "1961-01-01"
#' @param maxdate last day of modelling time period as \code{Date}- object. Optional, default is "2020-12-31"
#' @param path_std path to standard locations directory
#' @param path_climdb path to climate-rds files
#'
#' @import data.table
#' @return Returns a named list (names from \code{df.ids$ID_custom}) of climate data
#' @examples
#' fnc_get_clim(df.ids = test.ids.bds, mindate = as.Date("2010-01-01"), maxdate = as.Date("2011-12-31"))
#'
#' @export

fnc_get_clim <- function(df.ids,
                         mindate = as.Date("1961-01-01"),
                         maxdate = as.Date("2020-12-31"),
                         path_std = "R:/klima/whh/brook90_input/locations",
                         clim_dir = "R:/klima/whh/brook90_input/uhh_historische_referenz_v2020/rds/") {

  # IDs okay? ---------- ####
  # sort dfs according to IDs
  if(!"ID" %in% colnames(df.ids)){
    df.ids$ID <- 1:nrow(df.ids)
  }

  df.clim.ids <- fnc_relateCoords(df.ids = df.ids,
                                  path_std = path_std)


  dt.out <- data.table()
  for(tranche in sort(unique(df.clim.ids$tranche))){

    clim_dir_tmp <- paste0(clim_dir, "tr", tranche, "/")
    # preselection of ids
    ids_in_tranche <- unique(as.character(df.clim.ids[which(df.clim.ids$tranche == tranche), "id_standard"]))

    for(id_standard in ids_in_tranche){

      # ind <- which(df.clim.ids$id_standard == id_standard)
      # ID_custom = df.clim.ids[ind, "ID_custom"]
      # ID = df.clim.ids[ind, "ID"]

      # load and convert to data.table
      dt.clim.tmp <- readRDS(paste0(clim_dir_tmp, id_standard, ".rds"))

      if(str_detect(clim_dir_tmp, "ECE")){
        dates_missing = readRDS("J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/UHH/datesmis.rds")
        dt.clim.tmp[34333:34698,"date"] <- as.numeric(dates_missing)
      }

      dt.clim.tmp <- as.data.table(dt.clim.tmp)

      # add ID_custom and dates, remove sddm
      # dt.clim.tmp[,"ID_custom" := rep(ID_custom, nrow(dt.clim.tmp))]
      # dt.clim.tmp[,"ID" := rep(ID, nrow(dt.clim.tmp))]
      dt.clim.tmp[,"id_standard" := id_standard]

      if(maxdate > as.Date("2020-12-31")){
        dt.clim.tmp[,"dates" := as.Date(paste(str_sub(date, 1,4),
                                               str_sub(date, 5,6),
                                               str_sub(date, 7,8),
                                               sep = "-"))]
        dt.clim.tmp[,c("date") := NULL]

        setnames(dt.clim.tmp,
                 old = c("sgz_cor", "prz_cor", "tmx_cor", "tav_cor", "tmn_cor", "wsp_cor", "vap_cor"),
                 new = c("globrad", "prec", "tmax", "tmean", "tmin", "windspeed", "vappres"))

      }else{
        dt.clim.tmp[,"dates" := as.Date(paste(year, month, day, sep = "-"))]
        dt.clim.tmp[,c("sddm", "year", "month", "day") := NULL]

        setnames(dt.clim.tmp,
                 old = c("grhds", "rrds", "tadx", "tadm", "tadn", "wsdm"),
                 new = c("globrad", "prec", "tmax", "tmean", "tmin", "windspeed"))
      }

      # filter dates
      dt.clim.tmp <- dt.clim.tmp[dates>= mindate & dates <= maxdate]

      # names and units
      cols_0_1 <- c("globrad", "prec", "tmean", "tmax", "tmin", "windspeed")
      dt.clim.tmp[ , (cols_0_1) := lapply(.SD, "*", 0.1), .SDcols = cols_0_1]
      dt.clim.tmp[ , vappres := vappres * 0.01]

      setorder(dt.clim.tmp, dates)
      dt.clim.tmp <- dt.clim.tmp[,.(id_standard, dates, globrad, prec, tmean, tmin, tmax, windspeed, vappres)]
      dt.out <- rbind(dt.out, dt.clim.tmp)
    }
  }

  ls.clim <- left_join(df.clim.ids[c("ID", "ID_custom", "id_standard")], dt.out, by = "id_standard") %>%
    arrange(ID, dates) %>%
    group_split(ID)

  names(ls.clim) <- unlist(lapply(ls.clim, function(x){x$ID_custom[[1]]}))

  # return resulting list
  return(ls.clim)
}
