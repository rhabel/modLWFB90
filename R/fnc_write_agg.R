#' Function to aggregate and write data from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for. \cr In comparison to \code{\link{fnc_write}}, which only reduces the columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page), this function enables aggregation over vegperiod and monthly, plus a more detailed selection of drought indices. See detail section.\cr\cr IMPORTANT: FOR RUNNING THIS AGGREGATE FUNCTION, \code{output} in \code{run_multiside_LWFB90} MUST BE SET TO A \code{df.output} AS SET BY THE CODE IN THE EXAMPLE SECTION \cr The function writes .RData files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. This "step-in-between" is necessary because SQLite does not support parallel writing.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param aggr_tp a string containing the desired aggregation time period. Can be \code{monthly},  \code{vegper}, or  \code{monthly_vegper}. The latter does both.
#' @param col_select_vp a string containing the desired columns from the vegperiod-aggregation (see details)
#' @param col_select_mon a string containing the desired columns from the monthly-aggregation (see details)
#' @param dir_name directory for tmp files, if \code{NA} as in default, results are returned to console
#'
#' @return Returns the desired output to .Rdata files
#'
#' @section Vegperiod and monthly outputs:
#' For a complete list of possible output types plus description, see \code{"U:/db_brook90_output/whh_db_documentation"}
#'
#' @examples
#' df.output <- set_outputLWFB90()
#' df.output[,] <- 0L
#' df.output[c("Evap", "Swat"), c("Day")] <- 1
#' df.output[c("Flow"), c("Mon")] <- 1
#'
#' @import data.table
#' @export

fnc_write_agg <- function(x,
                          aggr_tp,
                          col_select_vp = NA,
                          col_select_mon = NA,
                          dir_name = NA){


  # soil.df <- ls.soil[[1]]
  # colnames(soil.df) <- tolower(colnames(soil.df))
  # id_run <- ls.soil[[1]]$ID_custom[1]

  # get
  soil.df <- get("soil", envir = parent.frame(3))
  param_std <- get("param_b90", envir = parent.frame(3))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]


  colnames(x$FLOWMON.ASC) <- toupper(colnames(x$FLOWMON.ASC))
  colnames(x$EVAPDAY.ASC) <- toupper(colnames(x$EVAPDAY.ASC))
  colnames(x$SWATDAY.ASC) <- toupper(colnames(x$SWATDAY.ASC))
  colnames(x$FLOWDAY.ASC) <- toupper(colnames(x$FLOWDAY.ASC))

  # Aggregierung: ...
  x$swat.profile <- Aggregate.SWAT.ASC(SWATi = x$SWATDAY.ASC, soil = soil.df)

  if(stringr::str_detect(aggr_tp, "monthly")){
    output_monthly <- data.table(ID_custom = id_run,
                                 x$FLOWMON.ASC[,list(YR, MO, FLOW,SLFL,BYFL,VRFLN,DSFL,
                                                     SURFRUNOFF = ifelse(abs(round(FLOW-DSFL-BYFL-VRFLN,1))>0.1,
                                                                         round(FLOW-DSFL-BYFL-VRFLN,1), 0))],
                                 Evap.DailyToMonthly(x$EVAPDAY.ASC)[,-c(1,2), with=FALSE],
                                 SWATProfile.DailyToMonthly(x$swat.profile)[,-c(1,2), with=F])

    setnames(output_monthly, names(output_monthly), tolower(names(output_monthly)))
  }

  if(stringr::str_detect(aggr_tp, "vegper")){
    output_vp <- data.table(ID_custom = id_run,
                            Flow.DailyToVegper(dat = x$FLOWDAY.ASC,
                                                          vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                          vp.start = param_std$budburstdoy,
                                                          vp.end = param_std$leaffalldoy,
                                                          bypar = param_std$bypar),
                            Evap.DailyToVegper(dat = x$EVAPDAY.ASC,
                                               vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                               vp.start = param_std$budburstdoy,
                                               vp.end = param_std$leaffalldoy)[,-c(1:3), with = F],
                            SWATProfile.DailyToVegper(dat = x$swat.profile,
                                                      vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                      vp.start = param_std$budburstdoy,
                                                      vp.end = param_std$leaffalldoy)[,-1, with=F])
    setnames(output_vp, names(output_vp), tolower(names(output_vp)))
  }


  # Output-Selection ...

  if(!any(is.na(col_select_vp))){
    keep <- c("id_custom", "yr", "vpstartdoy", "vpenddoy",
              col_select_vp)
    output_vp <- output_vp[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_mon))){
    keep <- c("id_custom", "yr", "mo",
              col_select_mon)
    output_monthly <- output_monthly[, keep, with = FALSE]
  }

  ls.out <- list()
  if(is.na(dir_name)){
    if(stringr::str_detect(aggr_tp, "vegper")){
      ls.out <- append(ls.out, list(output_vp))
      names(ls.out)[length(ls.out)] <- "output_vegper"
    }
    if(stringr::str_detect(aggr_tp, "monthly")){
      ls.out <- append(ls.out, list(output_monthly))
      names(ls.out)[length(ls.out)] <- "output_monthly"
    }

    return(ls.out)
  }else{
    # write to tmp
    if(stringr::str_detect(aggr_tp, "monthly")){

      if(!dir.exists(paste0(dir_name, "/monthly/"))){
        dir.create(paste0(dir_name, "/monthly/"), recursive = T)}

      save(output_monthly,
           file = paste0(dir_name, "/monthly/", id_run, ".RData"))

    }

    if(stringr::str_detect(aggr_tp, "vegper")){

      if(!dir.exists(paste0(dir_name, "/vegper/"))){
        dir.create(paste0(dir_name, "/vegper/"), recursive = T)}

      save(output_vp,
           file = paste0(dir_name, "/vegper/", id_run, ".RData"))

    }
  }
}
#
# b90.result <- run_LWFB90(options_b90 = options_b90,
#                          param_b90 = param_b90,
#                          climate = slb1_meteo,
#                          soil = ls.soil[[1]],
#                          output = df.output,
#                          output_fun = fnc_write_agg,
#
#                          aggr_tp = "vegper",
#                          dir_name = paste0(path_output, "tmp/"))
