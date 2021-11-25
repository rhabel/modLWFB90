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


Flow.DailyToVegper <- function(dat, vp.year, vp.start, vp.end, bypar) {

  setDT(dat)
  vp <- data.table(YR = vp.year, start = vp.start, end = vp.end)
  setkey(dat, YR)
  setkey(vp, YR)

  dat <- dat[vp]

  if(bypar == 0){
    dat_vp <- dat[which(DOY >= start & DOY <= end),
                  list(
                    VPSTARTDOY = start[1],
                    VPENDDOY = end[1],
                    FLOW = sum(FLOW),
                    SLFL = sum(SLFL),
                    BYFL = sum(BYFL),
                    VRFLN = sum(VRFLN),
                    DSFL = sum(DSFL),
                    SURFRUNOFF = sum(BYFL+SRFL)
                  ),
                  by = list(YR)]
  }else{
    dat_vp <- dat[which(DOY >= start & DOY <= end),
                  list(
                    VPSTARTDOY = start[1],
                    VPENDDOY = end[1],
                    FLOW = sum(FLOW),
                    SLFL = sum(SLFL),
                    BYFL = sum(BYFL),
                    VRFLN = sum(VRFLN),
                    DSFL = sum(DSFL),
                    SURFRUNOFF = sum(SRFL)
                  ),
                  by = list(YR)]
  }


  return(dat_vp)

}
