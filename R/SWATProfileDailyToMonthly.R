#' Internal Helper SWATProfile.DailyToMonthly
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


SWATProfile.DailyToMonthly <- function(dat) {
  setDT(dat)
  dat_month <- dat[,list(
    #SWAT
    SWAT_prf_avg = round(mean(SWAT_prf),1),
    SWAT_prf_min = round(min(SWAT_prf),1),
    SWAT_prf_end = SWAT_prf[which.max(da)],

    SWAT_we_avg = round(mean(SWAT_we),1),
    SWAT_we_min = round(min(SWAT_we),1),
    SWAT_we_end = SWAT_we[which.max(da)],
    SWAT_090_avg = round(mean(SWAT_090),1),
    SWAT_090_min = round(min(SWAT_090),1),
    SWAT_090_end = SWAT_090[which.max(da)],
    SWAT_060_avg = round(mean(SWAT_060),1),
    SWAT_060_min = round(min(SWAT_060),1),
    SWAT_060_end = SWAT_060[which.max(da)],
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

    #MISC
    # ADEF_avg = round(mean(ADEF),1),
    # ADEF_max = round(max(ADEF),1),
    # AWAT40_avg = round(mean(AWAT40),1),
    # AWAT40_min = round(min(AWAT40),1),

    #PSI
    PSIlogmean_we_avg = round(mean(PSIlogmean_we),1),
    PSIlogmean_we_min = round(min(PSIlogmean_we),1),
    PSIlogmean_090_avg = round(mean(PSIlogmean_090),1),
    PSIlogmean_090_min = round(min(PSIlogmean_090),1),
    PSIlogmean_060_avg = round(mean(PSIlogmean_060),1),
    PSIlogmean_060_min = round(min(PSIlogmean_060),1),

    # #StagWat
    # Days_StagWat30cm = sum(DepthStagWater>=-0.3),
    # Durations_StagWat30cm = paste((rle(DepthStagWater >= -0.3)$lengths[rle(DepthStagWater >= -0.3)$values]), collapse = " "),
    #PSI-Stress
    Days_PsiWe_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psiwe_lower1200 = paste((rle(PSIlogmean_we < -1200)$lengths[rle(PSIlogmean_we < -1200)$values]), collapse = " "),
    Defsum_PsiWe_lower1200 = round(sum( (PSIlogmean_we+1200) * (PSIlogmean_we < -1200)),2),

    Days_Psi090_lower1200 = sum(PSIlogmean_090 < -1200),
    Durations_Psi90_lower1200 = paste((rle(PSIlogmean_090 < -1200)$lengths[rle(PSIlogmean_090 < -1200)$values]), collapse = " "),
    Defsum_Psi090_lower1200 = round(sum( (PSIlogmean_090+1200) * (PSIlogmean_090 < -1200)),2),

    Days_Psi060_lower1200 = sum(PSIlogmean_060 < -1200),
    Durations_Psi60_lower1200 = paste((rle(PSIlogmean_060 < -1200)$lengths[rle(PSIlogmean_060 < -1200)$values]), collapse = " "),
    Defsum_Psi060_lower1200 = round(sum( (PSIlogmean_060+1200) * (PSIlogmean_060 < -1200)),2),

    #REW-STRESS
    Days_RELAWATwe_lower40 = sum(RELAWAT_we < 0.4),
    Durations_RELAWATwe_lower40 = paste((rle(RELAWAT_we < 0.4)
                                         $lengths[rle(RELAWAT_we < 0.4)$values]), collapse = " "),
    Defsum_RELAWATwe_lower40 = round(sum((1 - (RELAWAT_we/0.4))*(RELAWAT_we < 0.4)),2),

    Days_RELAWAT0100_lower40 = sum(RELAWAT_0100 < 0.4),
    Durations_RELAWAT0100_lower40 = paste((rle(RELAWAT_0100 < 0.4)
                                           $lengths[rle(RELAWAT_0100 < 0.4)$values]), collapse = " "),
    Defsum_RELAWAT0100_lower40 = round(sum((1 - (RELAWAT_0100/0.4))*(RELAWAT_0100 < 0.4)),2),

    Days_RELAWAT060_lower40 = sum(RELAWAT_060 < 0.4),
    Durations_RELAWAT060_lower40 = paste((rle(RELAWAT_060 < 0.4)
                                          $lengths[rle(RELAWAT_060 < 0.4)$values]), collapse = " "),
    Defsum_RELAWAT060_lower40 = round(sum((1 - (RELAWAT_060/0.4))*(RELAWAT_060 < 0.4)),2)
  ),
  by=list(YR, mo)]
  return(dat_month)
}
