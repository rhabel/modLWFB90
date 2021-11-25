#' Internal Helper Prec.DailyToVegper
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


Prec.DailyToVegper <- function(dat, vp.year, vp.start, vp.end, bypar) {

  setDT(dat)
  vp <- data.table(YR = vp.year, start = vp.start, end = vp.end)
  setkey(dat, YR)
  setkey(vp, YR)

  dat <- dat[vp]

  dat <- dat[which(DOY >= start & DOY <= end),
             list(
               VPSTARTDOY = start[1],
               VPENDDOY = end[1],
               PREC = sum(PREC)
             ),
             by = list(YR)]

  return(dat)

}
