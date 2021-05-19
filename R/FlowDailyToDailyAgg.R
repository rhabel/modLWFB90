#' Internal Helper Flow.DailyToDailyAgg
#'
#' ...
#'
#' @param ...
#'
#' @return Returns the desired output
#'
#'
#' @import data.table
#' @export


Flow.DailyToDailyAgg <- function(dat, bypar) {

  setDT(dat)

  keepcols <- c("YR", "MO", "DA", "FLOW", "BYFL", "VRFLN", "DSFL")

  dat_day <- dat[, ..keepcols]


  if(bypar == 0){
    dat_day[, "SURFRUNOFF" := BYFL]

  }else{
    dat_day[, "SURFRUNOFF" := FLOW-DSFL-BYFL-VRFLN]

  }

  return(dat_day)

}
