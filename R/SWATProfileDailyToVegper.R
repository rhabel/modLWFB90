#' Internal Helper SWATProfile.DailyToVegper
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


SWATProfile.DailyToVegper <- function(dat, vp.year, vp.start, vp.end) {

  setDT(dat)
  setkey(dat, YR)
  vp <- data.table::data.table(YR = vp.year, start = vp.start, end = vp.end)
  data.table::setkey(vp, YR)

  dat <- merge(dat,vp, by = "YR")
  dat_vp <- dat[which(DOY >= start & DOY <= end),list(
    #SWAT
    SWAT_prf_avg = round(mean(SWAT_prf),1),
    SWAT_prf_min = round(min(SWAT_prf),1),
    SWAT_we_avg = round(mean(SWAT_we),1),
    SWAT_we_min = round(min(SWAT_we),1),
    SWAT_0100_avg = round(mean(SWAT_0100),1),
    SWAT_0100_min = round(min(SWAT_0100),1),
    SWAT_060_avg = round(mean(SWAT_060),1),
    SWAT_060_min = round(min(SWAT_060),1),
    #AWAT
    AWAT_we_avg = round(mean(AWAT_we),1),
    AWAT_we_min = round(min(AWAT_we),1),
    AWAT_0100_avg = round(mean(AWAT_0100),1),
    AWAT_0100_min = round(min(AWAT_0100),1),
    AWAT_060_avg = round(mean(AWAT_060),1),
    AWAT_060_min = round(min(AWAT_060),1),
    #RELAWAT
    RELAWAT_we_avg = round(mean(RELAWAT_we),4),
    RELAWAT_we_min = round(min(RELAWAT_we),4),
    RELAWAT_0100_avg = round(mean(RELAWAT_0100),4),
    RELAWAT_0100_min = round(min(RELAWAT_0100),4),
    RELAWAT_060_avg = round(mean(RELAWAT_060),4),
    RELAWAT_060_min = round(min(RELAWAT_060),4),
    # #MISC
    # ADEF_avg = round(mean(ADEF),1),
    # ADEF_max = round(max(ADEF),1),
    # AWAT40_avg = round(mean(AWAT40),1),
    # AWAT40_min = round(min(AWAT40),1),
    #PSI
    PSIlogmean_we_avg = round(mean(PSIlogmean_we),1),
    PSIlogmean_we_min = round(min(PSIlogmean_we),1),
    PSIlogmean_0100_avg = round(mean(PSIlogmean_0100),1),
    PSIlogmean_0100_min = round(min(PSIlogmean_0100),1),
    PSIlogmean_060_avg = round(mean(PSIlogmean_060),1),
    PSIlogmean_060_min = round(min(PSIlogmean_060),1),
    # #StagWat-Stress
    # Days_StagWat30cm = sum(DepthStagWater >= -0.3),
    # Durations_StagWat30cm = paste((rle(DepthStagWater >= -0.3)$lengths[rle(DepthStagWater >= -0.3)$values]), collapse = " "),

    #PSI-Stress
    Days_PsiWe_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psiwe_lower1200 = paste((rle(PSIlogmean_we < -1200)$lengths[rle(PSIlogmean_we < -1200)$values]), collapse = " "),
    Defsum_PsiWe_lower1200 = round(sum((PSIlogmean_we + 1200) * (PSIlogmean_we < -1200)),1),

    Days_Psi0100_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psi0100_lower1200 = paste((rle(PSIlogmean_0100 < -1200)$lengths[rle(PSIlogmean_0100 < -1200)$values]), collapse = " "),
    Defsum_Psi0100_lower1200 = round(sum((PSIlogmean_0100 + 1200) * (PSIlogmean_0100 < -1200)),1),

    Days_Psi060_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psi60_lower1200 = paste((rle(PSIlogmean_060 < -1200)$lengths[rle(PSIlogmean_060 < -1200)$values]), collapse = " "),
    Defsum_Psi060_lower1200 = round(sum((PSIlogmean_060 + 1200) * (PSIlogmean_060 < -1200)),1),

    #REW-STRESS
    #Wurzelraum
    Days_RELAWATwe_lower40 = sum(RELAWAT_we < 0.4),
    Durations_RELAWATwe_lower40 = paste((rle(RELAWAT_we < 0.4)
                                         $lengths[rle(RELAWAT_we < 0.4)$values]), collapse = " "),
    Defsum_RELAWATwe_lower40 = round(sum((1 - (RELAWAT_we / 0.4)) * (RELAWAT_we < 0.4)),3),
    #0-90
    Days_RELAWAT0100_lower40 = sum(RELAWAT_0100 < 0.4),
    Durations_RELAWAT0100_lower40 = paste((rle(RELAWAT_0100 < 0.4)
                                           $lengths[rle(RELAWAT_0100 < 0.4)$values]), collapse = " "),
    Defsum_RELAWAT0100_lower40 = round(sum((1 - (RELAWAT_0100 / 0.4)) * (RELAWAT_0100 < 0.4)),3),
    #0-60
    Days_RELAWAT060_lower40 = sum(RELAWAT_060 < 0.4),
    Durations_RELAWAT060_lower40 = paste((rle(RELAWAT_060 < 0.4)
                                          $lengths[rle(RELAWAT_060 < 0.4)$values]), collapse = " "),
    Defsum_RELAWAT060_lower40 = round(sum((1 - (RELAWAT_060 / 0.4)) * (RELAWAT_060 < 0.4)),3)
  ),
  by = list(YR)]
  return(dat_vp)
}
