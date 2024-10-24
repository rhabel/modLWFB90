#' Internal Helper Evap.DailyToDailyAgg
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


Evap.DailyToDailyAgg <- function(dat) {

  setDT(dat)

  dat_day <- dat[,list(

    EVAPOTR = EVAP,
    TRAN = TRAN,
    INTV = IRVP + ISVP,
    SLVP = SLVP,
    SNVP = SNVP,

    PTRAN = PTRAN,
    PSLVP = PSLVP,

    TDIFF = PTRAN-TRAN,
    TRATIO = round(ifelse(PTRAN > 0, TRAN / PTRAN, 1),1)
  )]
  return(dat_day)

}
