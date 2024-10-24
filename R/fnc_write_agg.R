#' Function to aggregate and write data from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for. \cr In comparison to \code{\link{fnc_write}}, which only reduces the columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page), this function enables aggregation over vegperiod and monthly, plus a more detailed selection of drought indices. See detail section.\cr\cr IMPORTANT: FOR RUNNING THIS AGGREGATE FUNCTION, \code{output} in \code{run_multiside_LWFB90} MUST BE SET TO A \code{df.output} AS SET BY THE CODE IN THE EXAMPLE SECTION \cr The function writes .rds files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. This "step-in-between" is necessary because SQLite does not support parallel writing.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param aggr_tp a string containing the desired aggregation time period. Can be \code{daily},  \code{monthly}, \code{vegper}, \code{yearly}, or any combination of the four (i.e. \code{monthly_vegper}). The latter does all terms that are detectable within the string.
#' @param col_select_day a string containing the desired columns from the daily-aggregation (see details)
#' @param col_select_mon a string containing the desired columns from the monthly-aggregation (see details)
#' @param col_select_vp a string containing the desired columns from the vegperiod-aggregation (see details)
#' @param col_select_yr a string containing the desired columns from the yearly-aggregation (see details)
#' @param dir_name directory for tmp files, if \code{NA} as in default, results are returned to console
#'
#' @return Returns the desired output to .rds files
#'
#' @section Output column selection:
#' IT IS HIGHLY RECOMMENDED TO MAKE A SUBSELECTION, OR THERE WILL BE A LOT (>100) OF COLUMNS.
#' For a complete list of possible output types plus description, see \code{"U:/db_brook90_output/whh_db_documentation"}
#'
#' @examples
#' # setting for df.output:
#' df.output <- set_outputLWFB90()
#' df.output[,] <- 0L
#' df.output[c("Abov", "Evap", "Flow", "Swat"), c("Day")] <- 1
#'
#' # for full example, see help page of ?fnc_write_to_sql
#'
#' @import data.table
#' @export

fnc_write_agg <- function(x,
                          aggr_tp,
                          col_select_day = NA,
                          col_select_mon = NA,
                          col_select_vp = NA,
                          col_select_yr = NA,
                          dir_name = NA){


  # soil.df <- soil
  # colnames(soil.df) <- tolower(colnames(soil.df))
  # id_name <- soil$id_custom[1]
  # id_num <- soil$id[1]
  # param_std <- param_b90

  # get
  soil.df <- get("soil", envir = parent.frame(3))
  param_std <- get("param_b90", envir = parent.frame(3))
  id_name <- get("soil", envir = parent.frame(3))$id_custom[1]
  id_num <- get("soil", envir = parent.frame(3))$id[1]

  colnames(x$EVAPDAY.ASC) <- toupper(colnames(x$EVAPDAY.ASC))
  colnames(x$SWATDAY.ASC) <- toupper(colnames(x$SWATDAY.ASC))
  colnames(x$FLOWDAY.ASC) <- toupper(colnames(x$FLOWDAY.ASC))
  colnames(x$ABOVDAY.ASC) <- toupper(colnames(x$ABOVDAY.ASC))

  # Aggregierung: ...
  x$swat.profile <- modLWFB90:::Aggregate.SWAT.ASC(SWATi = x$SWATDAY.ASC, soil = soil.df)

  if(stringr::str_detect(aggr_tp, "daily")){
    output_daily <- data.table(ID = id_num,
                               ID_custom = id_name,
                               coords_x = param_std$coords_x,
                               coords_y = param_std$coords_y,
                               x$ABOVDAY.ASC[,list(YR, MO, DA,
                                                   PREC = RFAL+SFAL,
                                                   BSTN = RFAL+SFAL-RINT-SINT)],
                               x$FLOWDAY.ASC[,list(FLOW, SRFL, SLFL, BYFL, DSFL, VRFLN)],
                               Evap.DailyToDailyAgg(dat = x$EVAPDAY.ASC),
                               x$swat.profile[,-c(1:4), with=F])
    setnames(output_daily, names(output_daily), tolower(names(output_daily)))
    colnames(output_daily)[c(1, 2)] <- c("ID","ID_custom")
  }

  if(stringr::str_detect(aggr_tp, "monthly")){
    output_monthly <- data.table(ID = id_num,
                                 ID_custom = id_name,
                                 coords_x = param_std$coords_x,
                                 coords_y = param_std$coords_y,
                                 Prec.DailyToMonthly(dat = x$ABOVDAY.ASC)[,list(YR, MO, PREC, BSTN)],
                                 Flow.DailyToMonthly(dat = x$FLOWDAY.ASC)[,-c(1,2), with=FALSE],
                                 Evap.DailyToMonthly(dat = x$EVAPDAY.ASC)[,-c(1,2), with=FALSE],
                                 SWATProfile.DailyToMonthly(x$swat.profile)[,-c(1,2), with=F])

    setnames(output_monthly, names(output_monthly), tolower(names(output_monthly)))
    colnames(output_monthly)[c(1, 2)] <- c("ID","ID_custom")
  }

  if(stringr::str_detect(aggr_tp, "vegper")){
    output_vegper <- data.table(ID = id_num,
                                ID_custom = id_name,
                                coords_x = param_std$coords_x,
                                coords_y = param_std$coords_y,
                                Prec.DailyToVegper(dat = x$ABOVDAY.ASC,
                                                   vp.year = min(x$FLOWDAY.ASC$YR):max(x$FLOWDAY.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy),
                                Flow.DailyToVegper(dat = x$FLOWDAY.ASC,
                                                   vp.year = min(x$FLOWDAY.ASC$YR):max(x$FLOWDAY.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy)[,-c(1:3), with = F],
                                Evap.DailyToVegper(dat = x$EVAPDAY.ASC,
                                                   vp.year = min(x$FLOWDAY.ASC$YR):max(x$FLOWDAY.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy)[,-c(1:3), with = F],
                                SWATProfile.DailyToVegper(dat = x$swat.profile,
                                                          vp.year = min(x$FLOWDAY.ASC$YR):max(x$FLOWDAY.ASC$YR),
                                                          vp.start = param_std$budburstdoy,
                                                          vp.end = param_std$leaffalldoy)[,-1, with=F])
    setnames(output_vegper, names(output_vegper), tolower(names(output_vegper)))
    colnames(output_vegper)[c(1, 2)] <- c("ID","ID_custom")

  }

  if(stringr::str_detect(aggr_tp, "yearly")){
    output_yearly <- data.table(ID = id_num,
                                ID_custom = id_name,
                                coords_x = param_std$coords_x,
                                coords_y = param_std$coords_y,
                                Prec.DailyToYearly(dat = x$ABOVDAY.ASC),#[,list(YR, PREC, BSTN)],
                                Flow.DailyToYearly(dat = x$FLOWDAY.ASC)[,-1, with=F],
                                Evap.DailyToYearly(dat = x$EVAPDAY.ASC)[,-1, with=F],
                                SWATProfile.DailyToYearly(dat = x$swat.profile)[,-1, with=F])
    setnames(output_yearly, names(output_yearly), tolower(names(output_yearly)))
    colnames(output_yearly)[c(1, 2)] <- c("ID","ID_custom")

  }

  # Output-Selection ...

  if(!any(is.na(col_select_vp))){
    keep <- c("ID", "ID_custom", "coords_x", "coords_y", "yr", "vpstartdoy", "vpenddoy",
              col_select_vp)
    output_vegper <- output_vegper[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_mon))){
    keep <- c("ID", "ID_custom", "coords_x", "coords_y", "yr", "mo",
              col_select_mon)
    output_monthly <- output_monthly[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_yr))){
    keep <- c("ID", "ID_custom", "coords_x", "coords_y", "yr",
              col_select_yr)
    output_yearly <- output_yearly[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_day))){
    keep <- c("ID", "ID_custom", "coords_x", "coords_y", "yr", "mo", "da",
              col_select_day)
    output_daily <- output_daily[, keep, with = FALSE]
  }

  ls.out <- list()
  if(is.na(dir_name)){
    if(stringr::str_detect(aggr_tp, "daily")){
      ls.out <- append(ls.out, list(output_daily))
      names(ls.out)[length(ls.out)] <- "output_daily"
    }
    if(stringr::str_detect(aggr_tp, "vegper")){
      ls.out <- append(ls.out, list(output_vegper))
      names(ls.out)[length(ls.out)] <- "output_vegper"
    }
    if(stringr::str_detect(aggr_tp, "monthly")){
      ls.out <- append(ls.out, list(output_monthly))
      names(ls.out)[length(ls.out)] <- "output_monthly"
    }
    if(stringr::str_detect(aggr_tp, "yearly")){
      ls.out <- append(ls.out, list(output_yearly))
      names(ls.out)[length(ls.out)] <- "output_yearly"
    }

    return(ls.out)
  }else{
    # write to tmp
    if(stringr::str_detect(aggr_tp, "daily")){

      if(!dir.exists(paste0(dir_name, "daily/"))){
        dir.create(paste0(dir_name, "daily/"), recursive = T)}

      saveRDS(output_daily,
              file = paste0(dir_name, "daily/", id_num, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "yearly")){

      if(!dir.exists(paste0(dir_name, "yearly/"))){
        dir.create(paste0(dir_name, "yearly/"), recursive = T)}

      saveRDS(output_yearly,
              file = paste0(dir_name, "yearly/", id_num, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "monthly")){

      if(!dir.exists(paste0(dir_name, "monthly/"))){
        dir.create(paste0(dir_name, "monthly/"), recursive = T)}

      saveRDS(output_monthly,
              file = paste0(dir_name, "monthly/", id_num, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "vegper")){

      if(!dir.exists(paste0(dir_name, "vegper/"))){
        dir.create(paste0(dir_name, "vegper/"), recursive = T)}

      saveRDS(output_vegper,
              file = paste0(dir_name, "vegper/", id_num, ".rds"))

    }
  }
}
#
# b90.result <- run_LWFB90(options_b90 = options_b90,
#                          param_b90 = param_b90,
#                          climate = ls.clim[[1]],
#                          soil = ls.soil[[1]],
#                          output = df.output,
#                          output_fun = fnc_write_agg,
#
#                          aggr_tp = "daily",
#                          dir_name = NA)
