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
  dat[, PREC := RFAL + SFAL]
  dat[, BSTN := RFAL + SFAL - RINT - SINT]

  dat_month <- dat[,list(PREC = sum(PREC),
                         BSTN = sum(BSTN)),
  by = list(YR, MO)]
  return(dat_month)
}
