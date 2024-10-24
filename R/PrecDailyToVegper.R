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


Prec.DailyToVegper <- function(dat, vp.year, vp.start, vp.end) {

  setDT(dat)
  dat[, PREC := RFAL + SFAL]
  dat[, BSTN := RFAL + SFAL - RINT - SINT]

  vp <- data.table(YR = vp.year, start = vp.start, end = vp.end)
  setkey(dat, YR)
  setkey(vp, YR)

  dat <- dat[vp]

  dat <- dat[which(DOY >= start & DOY <= end),
             list(
               VPSTARTDOY = start[1],
               VPENDDOY = end[1],
               PREC = sum(PREC),
               BSTN = sum(BSTN)
             ),
             by = list(YR)]

  return(dat)

}
