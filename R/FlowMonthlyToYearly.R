#' Internal Helper Flow.MonthlyToYearly
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


Flow.MonthlyToYearly <- function(dat, bypar) {

  setDT(dat)
  setkey(dat, YR)

  if(bypar == 0){
    dat_yr <- dat[,list(FLOW = sum(FLOW),
                        SLFL = sum(SLFL),
                        BYFL = sum(BYFL),
                        VRFLN = sum(VRFLN),
                        DSFL = sum(DSFL),
                        SURFRUNOFF = sum(BYFL + SRFL)
    ),
    by = list(YR)]

  }else{
    dat_yr <- dat[,list(FLOW = sum(FLOW),
                        SLFL = sum(SLFL),
                        BYFL = sum(BYFL),
                        VRFLN = sum(VRFLN),
                        DSFL = sum(DSFL),
                        SURFRUNOFF = sum(SRFL)
    ),
    by = list(YR)]

  }


  return(dat_yr)

}
