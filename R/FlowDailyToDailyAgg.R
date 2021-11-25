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

  if(bypar == 0){

    dat[, "SURFRUNOFF" := BYFL+SRFL]

    keepcols <- c("FLOW", "SLFL", "VRFLN", "DSFL", "SURFRUNOFF")

    dat <- dat[, ..keepcols]

  }else{
    dat[, "SURFRUNOFF" := SRFL]

    keepcols <- c("FLOW", "SLFL", "BYFL", "VRFLN", "DSFL", "SURFRUNOFF")

    dat <- dat[, ..keepcols]

  }

  return(dat)

}
