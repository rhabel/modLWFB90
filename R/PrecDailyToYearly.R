#' Internal Helper Prec.DailyToYearly
#' ...
#'
#' @param ...
#'
#' @return Returns the desired output
#'
#'
#' @import data.table
#' @export


Prec.DailyToYearly <- function(dat) {

  setDT(dat)
  setkey(dat, YR)

  dat_yr <- dat[,list(
    PREC = sum(PREC)),
  by = YR]
  return(dat_yr)

}
