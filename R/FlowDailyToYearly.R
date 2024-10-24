#' Internal Helper Flow.DailyToYearly
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


Flow.DailyToYearly <- function(dat) {

  setDT(dat)
  setkey(dat, YR)

    dat <- dat[,list(
      FLOW = sum(FLOW),
      SRFL = sum(SRFL),
      SLFL = sum(SLFL),
      BYFL = sum(BYFL),
      DSFL = sum(DSFL),
      VRFLN = sum(VRFLN)
    ),
    by = list(YR)]

  return(dat)

}
