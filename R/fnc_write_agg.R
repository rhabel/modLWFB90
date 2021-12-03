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
#' df.output[c("Budg", "Evap", "Flow", "Swat"), c("Day")] <- 1
#' df.output[c("Flow"), c("Mon")] <- 1
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
  # id_run <- soil$ID_custom[1]
  # param_std <- param_b90

  # get
  soil.df <- get("soil", envir = parent.frame(3))
  param_std <- get("param_b90", envir = parent.frame(3))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]


  colnames(x$FLOWMON.ASC) <- toupper(colnames(x$FLOWMON.ASC))
  colnames(x$EVAPDAY.ASC) <- toupper(colnames(x$EVAPDAY.ASC))
  colnames(x$SWATDAY.ASC) <- toupper(colnames(x$SWATDAY.ASC))
  colnames(x$FLOWDAY.ASC) <- toupper(colnames(x$FLOWDAY.ASC))
  colnames(x$BUDGDAY.ASC) <- toupper(colnames(x$BUDGDAY.ASC))

  # Aggregierung: ...
  x$swat.profile <- Aggregate.SWAT.ASC(SWATi = x$SWATDAY.ASC, soil = soil.df)

  if(stringr::str_detect(aggr_tp, "daily")){
    output_daily <- data.table(ID_custom = id_run,
                               x$BUDGDAY.ASC[,list(YR, MO, DA, PREC)],
                               Flow.DailyToDailyAgg(dat = x$FLOWDAY.ASC,
                                                    bypar = param_std$bypar),
                               Evap.DailyToDailyAgg(dat = x$EVAPDAY.ASC),
                               x$swat.profile[,-c(1:4), with=F])
    setnames(output_daily, names(output_daily), tolower(names(output_daily)))
  }

  if(stringr::str_detect(aggr_tp, "yearly")){
    output_yearly <- data.table(ID_custom = id_run,
                                Prec.DailyToYearly(dat = x$BUDGDAY.ASC),
                                Flow.MonthlyToYearly(dat = x$FLOWMON.ASC,
                                               bypar = param_std$bypar)[,-1, with=F],
                                Evap.DailyToYearly(dat = x$EVAPDAY.ASC)[,-1, with=F],
                                SWATProfile.DailyToYearly(dat = x$swat.profile)[,-1, with=F])
    setnames(output_yearly, names(output_yearly), tolower(names(output_yearly)))
  }

  if(stringr::str_detect(aggr_tp, "monthly")){
    output_monthly <- data.table(ID_custom = id_run,
                                 Prec.DailyToMonthly(x$BUDGDAY.ASC),
                                 x$FLOWMON.ASC[,list(FLOW,SLFL,BYFL,VRFLN,DSFL,
                                                     SURFRUNOFF = ifelse(param_std$bypar == 0,
                                                                         BYFL+SRFL, SRFL))],
                                 Evap.DailyToMonthly(x$EVAPDAY.ASC)[,-c(1,2), with=FALSE],
                                 SWATProfile.DailyToMonthly(x$swat.profile)[,-c(1,2), with=F])

    setnames(output_monthly, names(output_monthly), tolower(names(output_monthly)))
  }

  if(stringr::str_detect(aggr_tp, "vegper")){
    output_vegper <- data.table(ID_custom = id_run,
                                Prec.DailyToVegper(dat = x$BUDGDAY.ASC,
                                                   vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy)[,-c(1:3), with = F],
                                Flow.DailyToVegper(dat = x$FLOWDAY.ASC,
                                                   vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy,
                                                   bypar = param_std$bypar)[,-c(1:3), with = F],
                                Evap.DailyToVegper(dat = x$EVAPDAY.ASC,
                                                   vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                   vp.start = param_std$budburstdoy,
                                                   vp.end = param_std$leaffalldoy)[,-c(1:3), with = F],
                                SWATProfile.DailyToVegper(dat = x$swat.profile,
                                                          vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                          vp.start = param_std$budburstdoy,
                                                          vp.end = param_std$leaffalldoy)[,-1, with=F])
    setnames(output_vegper, names(output_vegper), tolower(names(output_vegper)))
  }


  # Output-Selection ...

  if(!any(is.na(col_select_vp))){
    keep <- c("id_custom", "yr", "vpstartdoy", "vpenddoy",
              col_select_vp)
    output_vegper <- output_vegper[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_mon))){
    keep <- c("id_custom", "yr", "mo",
              col_select_mon)
    output_monthly <- output_monthly[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_yr))){
    keep <- c("id_custom", "yr",
              col_select_yr)
    output_yearly <- output_yearly[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_day))){
    keep <- c("id_custom", "yr", "mo", "da",
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
    if(stringr::str_detect(aggr_tp, "yearly")){

      if(!dir.exists(paste0(dir_name, "yearly/"))){
        dir.create(paste0(dir_name, "yearly/"), recursive = T)}

      saveRDS(output_yearly,
              file = paste0(dir_name, "yearly/", id_run, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "yearly")){

      if(!dir.exists(paste0(dir_name, "yearly/"))){
        dir.create(paste0(dir_name, "yearly/"), recursive = T)}

      saveRDS(output_yearly,
              file = paste0(dir_name, "yearly/", id_run, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "monthly")){

      if(!dir.exists(paste0(dir_name, "monthly/"))){
        dir.create(paste0(dir_name, "monthly/"), recursive = T)}

      saveRDS(output_monthly,
              file = paste0(dir_name, "monthly/", id_run, ".rds"))

    }

    if(stringr::str_detect(aggr_tp, "vegper")){

      if(!dir.exists(paste0(dir_name, "vegper/"))){
        dir.create(paste0(dir_name, "vegper/"), recursive = T)}

      saveRDS(output_vegper,
              file = paste0(dir_name, "vegper/", id_run, ".rds"))

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
