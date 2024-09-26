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

  dat[, PREC := RFAL + SFAL]
  dat[, BSTN := RFAL + SFAL - RINT - SINT]

  dat_yr <- dat[,list(
    PREC = sum(PREC),
    BSTN = sum(BSTN)),
  by = YR]
  return(dat_yr)

}
