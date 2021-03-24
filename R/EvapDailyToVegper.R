#' Internal Helper Evap.DailyToVegper
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


Evap.DailyToVegper <- function(dat, vp.year, vp.start, vp.end) {

  setDT(dat)
  vp <- data.table(YR = vp.year, start = vp.start, end = vp.end)
  setkey(dat, YR)
  setkey(vp, YR)

  dat <- dat[vp]

  dat_vp <- dat[which(DOY >= start & DOY <= end),list(
    VPSTARTDOY = start[1],
    VPENDDOY = end[1],
    TRAN = sum(TRAN),
    INTV = sum(IRVP + ISVP),
    EVAP = sum(SLVP + SNVP),
    # FLOW = sum(FLOW),
    # ISVP = sum(ISVP),
    # SLVP = sum(SLVP),
    # SNVP = sum(SNVP),
    PTRAN = sum(PTRAN),
    PSLVP = sum(PSLVP),
    TDIFF = round(sum(PTRAN-TRAN),1),

    TRATIO_avg = round(mean(ifelse(PTRAN > 0, TRAN / PTRAN, 1)),3),
    TRATIO_min = round(min(ifelse(PTRAN > 0, TRAN / PTRAN, 1)),3),

    Days_TRATIO_lower50 = sum((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5),
    Durations_TRATIO_lower50 = paste((rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5)
                                      $lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5)$values]), collapse = " "),
    Defsum_TRATIO_lower50 = round(sum((1 - (ifelse(PTRAN > 0, TRAN / PTRAN, 1) / 0.5)) * (ifelse(PTRAN > 0, TRAN / PTRAN, 1) < 0.5)),3),

    Days_TRATIO_lower80 = sum((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8),
    Durations_TRATIO_lower80 = paste((rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)
                                      $lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$values]), collapse = " "),
    Defsum_TRATIO_lower80 = round(sum((1 - (ifelse(PTRAN > 0, TRAN / PTRAN, 1) / 0.8)) * (ifelse(PTRAN > 0, TRAN / PTRAN, 1) < 0.8)),3)
  ),
  by = list(YR)]
  return(dat_vp)

}
