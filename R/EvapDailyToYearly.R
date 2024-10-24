#' Internal Helper Evap.DailyToYearly
#' ...
#'
#' @param ...
#'
#' @return Returns the desired output
#'
#'
#' @import data.table
#' @export


Evap.DailyToYearly <- function(dat) {

  setDT(dat)
  setkey(dat, YR)

  dat_yr <- dat[,list(
    EVAPOTR = sum(EVAP),
    TRAN = sum(TRAN),
    INTV = sum(IRVP + ISVP),
    SLVP = sum(SLVP),
    SNVP = sum(SNVP),

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
    # Max_Durations_TRATIO_lower80 = ifelse(length((rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$values])) > 0,
    #                                                   max(rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$values]),0),
    Defsum_TRATIO_lower80 = round(sum((1 - (ifelse(PTRAN > 0, TRAN / PTRAN, 1) / 0.8)) * (ifelse(PTRAN > 0, TRAN / PTRAN, 1) < 0.8)),3)
  ),
  by = YR]
  return(dat_yr)

}
