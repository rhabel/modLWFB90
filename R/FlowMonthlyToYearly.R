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
                        BYFL = sum(BYFL),
                        VRFLN = sum(VRFLN),
                        DSFL = sum(DSFL),
                        SURFRUNOFF = sum(BYFL)
    ),
    by = list(YR)]

  }else{
    dat_yr <- dat[,list(FLOW = sum(FLOW),
                        BYFL = sum(BYFL),
                        VRFLN = sum(VRFLN),
                        DSFL = sum(DSFL),
                        SURFRUNOFF = sum(FLOW-DSFL-BYFL-VRFLN)
    ),
    by = list(YR)]

  }


  return(dat_yr)

}
