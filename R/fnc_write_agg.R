#' Function to aggregate and write data from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for. \cr In comparison to \code{\link{fnc_write}}, which only reduces the columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page), this function enables aggregation over vegperiod and monthly, plus a more detailed selection of drought indices. See detail section.\cr\cr IMPORTANT: FOR RUNNING THIS AGGREGATE FUNCTION, \code{output} in \code{run_multiside_LWFB90} MUST BE SET TO A \code{df.output} AS SET BY THE CODE IN THE EXAMPLE SECTION
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param aggr_tp a string containing the desired aggregation time period. Can be \code{monthly},  \code{vegper}, or  \code{monthly_vegper}. The latter does both.
#' @param col_select_vp a string containing the desired columns from the vegperiod-aggregation (see details)
#' @param col_select_mon a string containing the desired columns from the monthly-aggregation (see details)
#' @param db_name name and file path of the SQL-database
#'
#' @return Returns the desired output to the database directly.
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
#' @import data.table RSQLite
#' @export

fnc_write_agg <- function(x,
                          aggr_tp,
                          col_select_vp = NA,
                          col_select_mon = NA,
                          db_name){


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
  # colnames(x$MISCDAY.ASC) <- toupper(colnames(x$MISCDAY.ASC))

  # Aggregierung: ...
  x$swat.profile <- modLWFB90::Aggregate.SWAT.ASC(SWATi = x$SWATDAY.ASC, soil = soil.df)

  if(stringr::str_detect(aggr_tp, "monthly")){
    output_monthly <- data.table(ID_custom = id_run,
                                 x$FLOWMON.ASC[,list(YR, MO, FLOW,SLFL,BYFL,VRFLN,DSFL,SURFRUNOFF = FLOW-DSFL-BYFL-VRFLN)],
                                 modLWFB90::Evap.DailyToMonthly(x$EVAPDAY.ASC)[,-c(1,2), with=FALSE],
                                 modLWFB90::SWATProfile.DailyToMonthly(x$swat.profile)[,-c(1,2), with=F])

    setnames(output_monthly, names(output_monthly), tolower(names(output_monthly)))
  }

  if(stringr::str_detect(aggr_tp, "vegper")){
    output_vp <- data.table(ID_custom = id_run,
                            modLWFB90::Evap.DailyToVegper(dat = x$EVAPDAY.ASC,
                                               vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                               vp.start = param_std$budburstdoy,
                                               vp.end = param_std$leaffalldoy),
                            modLWFB90::SWATProfile.DailyToVegper(dat = x$swat.profile,
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

  # write to db
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_name)
  rest <- RSQLite::dbSendQuery(con, "PRAGMA busy_timeout=5000;")
  RSQLite::dbClearResult(rest)

  on.exit(RSQLite::dbDisconnect(con))

  repeat {
    rv <- try({
      RSQLite::dbWriteTable(con,
                            "soils",
                            soil.df[,-1],
                            append=T, overwrite = F, row.names=F)

      if(stringr::str_detect(aggr_tp, "monthly")){
        RSQLite::dbWriteTable(con,
                              "monthly",
                              output_monthly,
                              append=T, overwrite = F, row.names=F)
      }

      if(stringr::str_detect(aggr_tp, "vegper")){
        RSQLite::dbWriteTable(con,
                              "vegper",
                              output_vp,
                              append=T, overwrite = F, row.names=F)
      }
    })
    if(!is(rv, "try-error")) break
  }
  rv
}


