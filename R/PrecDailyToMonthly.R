#' Internal Helper Prec.DailyToMonthly
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


Prec.DailyToMonthly <- function(dat) {
  setDT(dat)
  dat_month <- dat[,list(PREC = sum(PREC)),
  by = list(YR, MO)]
  return(dat_month)
}
