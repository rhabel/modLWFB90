#' Internal Helper Flow.DailyToVegper
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


Flow.DailyToVegper <- function(dat, vp.year, vp.start, vp.end) {

  setDT(dat)
  vp <- data.table(YR = vp.year, start = vp.start, end = vp.end)
  setkey(dat, YR)
  setkey(vp, YR)

  dat <- dat[vp]

  dat <- dat[which(DOY >= start & DOY <= end),
                  list(
                    VPSTARTDOY = start[1],
                    VPENDDOY = end[1],

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
