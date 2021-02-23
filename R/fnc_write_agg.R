#' Function to aggregate and write data from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it to a database. This is what this function is made for. \cr In comparison to \code{\link{fnc_write}}, which only reduces the columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page), this function enables aggregation over vegperiod and monthly, plus a more detailed selection of drought indices. See detail section.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param aggr_tp a string containing the desired aggregation time period. Can be \code{monthly},  \code{vegper}, or  \code{monthly_vegper}. The latter does both.
#' @param col_select_vp a string containing the desired columns from the vegperiod-aggregation (see details)
#' @param col_select_mon a string containing the desired columns from the monthly-aggregation (see details)
#' @param db_name name and file path of the SQL-database
#'
#' @return Returns the desired output to the database directly.
#'
#' @section Vegperiod outputs:
#' \tabular{llcl}{
#' \strong{Name} \tab \strong{Description} \tab \strong{Unit} \cr
#' tran \tab transpiration \tab mm \cr
#' irvp \tab evaporation of intercepted rain \tab mm \cr
#' isvp \tab evaporation of intercepted snow \tab mm \cr
#' slvp \tab year \tab - \cr
#' ptran \tab potential transpiration \tab mm \cr
#' pslvp \tab potential soil evaporation \tab mm \cr
#' ...
#' }
#'
#'
#' @import data.table RSQLite
#' @export

fnc_write_agg <- function(x,
                          aggr_tp,
                          col_select_vp = NA,
                          col_select_mon = NA,
                          db_name){


  # soil.df <- ls.soil[[1]]
  # colnames(soil.df) <- tolower(colnames(soil.df))
  # id_run <- ls.soil[[1]]$ID_custom[1]

  # get
  soil.df <- get("soil", envir = parent.frame(3))
  param_std <- get("param_b90", envir = parent.frame(2))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]


  colnames(x$FLOWMON.ASC) <- toupper(colnames(x$FLOWMON.ASC))
  colnames(x$EVAPDAY.ASC) <- toupper(colnames(x$EVAPDAY.ASC))
  colnames(x$SWATDAY.ASC) <- toupper(colnames(x$SWATDAY.ASC))
  colnames(x$MISCDAY.ASC) <- toupper(colnames(x$MISCDAY.ASC))

  # Aggregierung: ...
  x$swat.profile <- Aggregate.SWAT.ASC(SWATi = x$SWATDAY.ASC, soil = soil.df)

  if(stringr::str_detect(aggr_tp, "monthly")){
    output_monthly <- data.table(ID_custom = id_run,
                                 x$FLOWMON.ASC[,list(YR, MO, FLOW,SLFL,BYFL,VRFLN,DSFL,SURFRUNOFF = FLOW-DSFL-BYFL-VRFLN)],
                                 Evap.DailyToMonthly(x$EVAPDAY.ASC)[,-c(1,2), with=FALSE],
                                 SWATProfile.DailyToMonthly(x$swat.profile)[,-c(1,2), with=F])

    setnames(output_monthly, names(output_monthly), tolower(names(output_monthly)))
  }

  if(stringr::str_detect(aggr_tp, "vegper")){
    output_vp <- data.table(ID_custom = id_run,
                            Evap.DailyToVegper(dat = x$EVAPDAY.ASC,
                                               vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                               vp.start = param_std$budburstdoy,
                                               vp.end = param_std$leaffalldoy),
                            SWATProfile.DailyToVegper(dat = x$swat.profile,
                                                      vp.year = min(x$FLOWMON.ASC$YR):max(x$FLOWMON.ASC$YR),
                                                      vp.start = param_std$budburstdoy,
                                                      vp.end = param_std$leaffalldoy)[,-1, with=F])
    setnames(output_vp, names(output_vp), tolower(names(output_vp)))
  }


  # Output-Selection ...

  if(!any(is.na(col_select_vp))){
    keep <- c("id_custom", "yr", "vpstartdoy", "vpenddoy",
              col_select_vp)
    output_vp <- output_vp[, keep, with = FALSE]
  }

  if(!any(is.na(col_select_mon))){
    keep <- c("id_custom", "yr", "mo",
              col_select_mon)
    output_monthly <- output_monthly[, keep, with = FALSE]
  }

  # write to db
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_name)
  rest <- RSQLite::dbSendQuery(con, "PRAGMA busy_timeout=5000;")
  RSQLite::dbClearResult(rest)

  on.exit(RSQLite::dbDisconnect(con))

  repeat {
    rv <- try({
      RSQLite::dbWriteTable(con,
                            "soils",
                            soil.df[,-1],
                            append=T, overwrite = F, row.names=F)

    })
    if(!is(rv, "try-error")) break
  }

  repeat {
    rv <- try({
      if(stringr::str_detect(aggr_tp, "monthly")){
        RSQLite::dbWriteTable(con,
                              "monthly",
                              output_monthly,
                              append=T, overwrite = F, row.names=F)
      }
    })
    if(!is(rv, "try-error")) break
  }

  repeat {
    rv <- try({
      if(stringr::str_detect(aggr_tp, "vegper")){
        RSQLite::dbWriteTable(con,
                              "vegper",
                              output_vp,
                              append=T, overwrite = F, row.names=F)
      }
    })
    if(!is(rv, "try-error")) break
  }
}


# Helperfunctions ----------------------

MvG.swc <- function(
  psi, #pressure head in hPa
  alpha, #MvG alpha
  n, # MvG n
  ThS,
  ThR,
  m = 1-1/n)
{
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) + ThR
  return(theta)
}

# aggregate over profile

Aggregate.SWAT.ASC <- function(SWATi, soil){

  # colnames(SWATi) <- toupper(colnames(SWATi))
  #create layer-column if not existing
  soil$nl <- 1:nrow(soil)

  setDT(soil)
  setDT(SWATi)

  soil[,c("thick","FK","PWP") := list(upper  - lower,
                                      MvG.swc(63, alpha / 100, npar, ths, thr) * (1 - gravel),
                                      MvG.swc(10^4.2, alpha / 100, npar, ths, thr) * (1 - gravel))]
  #MvG.swc(20000, alpha / 100, npar, ths, thr) * (1 - gravel))]

  soil[,c("nFK","FK.mm","PWP.mm", "nFK.mm" ) := list(FK - PWP,
                                                     FK * 100 * thick * 10,
                                                     PWP * 100 * thick * 10,
                                                     #PWPpsicr * 100 * thick * 10,
                                                     (FK-PWP) * 100 * thick * 10
  )]

  setkey(soil, nl)
  setkey(SWATi, NL)

  SWATi <- SWATi[soil,] # fast join soil hydraulic properties
  # key: nl, which are the SANDBELOW Layers ?

  # Schichtwerte   --------------------------------------------------------------------------------------------------------------
  # berechnen: relative Speicherfuellung, relative nutzbare Wasserverfuegbarkeit jeder Schicht
  SWATi[, SWATI := THETA * (1 - gravel) * 100 * thick * 10]
  SWATi[, AWAT := SWATI - PWP.mm]
  # SWATi[, AWATpsicr := SWATI - PWPpsicr.mm]
  # SWATi <- SWATi[order(SWATi$YR,SWATi$DOY,SWATi$NL),] dont sort, will loose thekey

  # 1.a)Tageswerte  Bodenwasserspeicher ########################

  SWAT.profil <- SWATi[,list(
    mo = MO[1], # Monat mitschleppen
    da = DA[1],
    SWAT_prf = round(sum(SWATI),1),
    SWAT_we = round(sum(SWATI * (rootden > 0)),1),
    SWAT_090 =  round(sum(SWATI * (upper <= 0 & lower >= -0.9)),1),
    SWAT_060 =  round(sum(SWATI * (upper <= 0 & lower >= -0.6)),1),
    AWAT_we =  round(sum(AWAT * (rootden > 0)),1),
    AWAT_0100 =  round(sum(AWAT * (upper <= 0 & lower >= -1.0)),1),
    AWAT_060 =  round(sum(AWAT * (upper <= 0 & lower >= -0.6)),1),
    RELAWAT_we =  round(sum(AWAT * (rootden > 0)) / sum(nFK.mm * (rootden > 0)),3),
    RELAWAT_0100 =  round(sum(AWAT * (upper <= 0 & lower >= -1.0)) / sum(nFK.mm * (upper <= 0 & lower >= -1.0)),3),
    RELAWAT_060 =  round(sum(AWAT * (upper <= 0 & lower >= -0.6)) / sum(nFK.mm * (upper <= 0 & lower >= -0.6)),3),
    #AWAT40 = round(sum(AWATpsicr * 0.6 * (rootden > 0)),1), #ich kriege nicht ganz dasselbe raus wie miscday.awat40!
    #AWAT40rw = round(sum(AWATpsicr * 0.6 * rootden/sum(rootden),1),

    RELSWAT_we =  round(sum(SWATI * (rootden > 0)) / sum(nFK.mm * (rootden > 0)),3),
    RELSWAT_0100 =  round(sum(SWATI * (upper <= 0 & lower >= -1.0)) / sum(nFK.mm * (upper <= 0 & lower >= -1.0)),3),
    RELSWAT_060 =  round(sum(SWATI * (upper <= 0 & lower >= -0.6)) / sum(nFK.mm * (upper <= 0 & lower >= -0.6)),3),

    PSIlogmean_we = round(-1*10 ^ (weighted.mean(log10(PSIMI * -10),
                                                 (ths * (1 - gravel) * 100 * thick * 10) * (rootden > 0)))
                          ,1),
    PSIlogmean_090 = round(-1 * 10 ^ (weighted.mean(log10(PSIMI * -10),
                                                    (ths * (1 - gravel) * 100 * thick * 10)*(upper <= 0 & lower >= -0.9)))
                           ,1),
    PSIlogmean_060 = round(-1 * 10 ^ (weighted.mean(log10(PSIMI * -10),
                                                    (ths * (1 - gravel) * 100 * thick * 10)*(upper <= 0 & lower >= -0.6)))
                           ,1),
    WaterTableDepth = -min(ifelse( -upper * (WETNES > 0.99) > 0,-upper * (WETNES > 0.99),Inf))
  ),
  by = list(YR, DOY)]

  return(SWAT.profil)
}


# aggregate over Vegper

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
    IRVP = sum(IRVP),
    ISVP = sum(ISVP),
    SLVP=sum(SLVP),
    SNVP = sum(SNVP),
    PTRAN = sum(PTRAN),
    PSLVP = sum(PSLVP),
    TDIFF = round(sum(PTRAN-TRAN),1),
    TRATIO_avg = round(mean(ifelse(PTRAN > 0, TRAN / PTRAN, 1)),3),
    TRATIO_min = round(mean(ifelse(PTRAN > 0, TRAN / PTRAN, 1)),3),
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
    SWAT_090_avg = round(mean(SWAT_090),1),
    SWAT_090_min = round(min(SWAT_090),1),
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
    PSIlogmean_090_avg = round(mean(PSIlogmean_090),1),
    PSIlogmean_090_min = round(min(PSIlogmean_090),1),
    PSIlogmean_060_avg = round(mean(PSIlogmean_060),1),
    PSIlogmean_060_min = round(min(PSIlogmean_060),1),
    # #StagWat-Stress
    # Days_StagWat30cm = sum(DepthStagWater >= -0.3),
    # Durations_StagWat30cm = paste((rle(DepthStagWater >= -0.3)$lengths[rle(DepthStagWater >= -0.3)$values]), collapse = " "),

    #PSI-Stress
    Days_PsiWe_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psiwe_lower1200 = paste((rle(PSIlogmean_we < -1200)$lengths[rle(PSIlogmean_we < -1200)$values]), collapse = " "),
    Defsum_PsiWe_lower1200 = round(sum((PSIlogmean_we + 1200) * (PSIlogmean_we < -1200)),1),

    Days_Psi090_lower1200 = sum(PSIlogmean_we < -1200),
    Durations_Psi90_lower1200 = paste((rle(PSIlogmean_090 < -1200)$lengths[rle(PSIlogmean_090 < -1200)$values]), collapse = " "),
    Defsum_Psi090_lower1200 = round(sum((PSIlogmean_090 + 1200) * (PSIlogmean_090 < -1200)),1),

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


# aggregate monthly

Evap.DailyToMonthly <- function(dat) {
  setDT(dat)
  dat_month <- dat[,list(EVAP= sum(EVAP),
                         TRAN = sum(TRAN),
                         IRVP = sum(IRVP),
                         ISVP = sum(ISVP),
                         SLVP=sum(SLVP),
                         SNVP = sum(SNVP),
                         PTRAN = sum(PTRAN),
                         PSLVP = sum(PSLVP),
                         TDIFF = round(sum(PTRAN-TRAN),1),

                         TRATIO_avg = round(mean(ifelse(PTRAN>0, TRAN/PTRAN,1)),3),
                         TRATIO_min = round(min(ifelse(PTRAN>0, TRAN/PTRAN,1)),3),
                         Days_TRATIO_lower50 = sum((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5),
                         Durations_TRATIO_lower50 = paste((rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5)
                                                           $lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.5)$values]), collapse = " "),
                         Defsum_TRATIO_lower50 = round(sum((1 - (ifelse(PTRAN > 0, TRAN / PTRAN, 1) / 0.5)) * (ifelse(PTRAN > 0, TRAN / PTRAN, 1) < 0.5)),3),

                         Days_TRATIO_lower80 = sum((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8),
                         Durations_TRATIO_lower80 = paste((rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)
                                                           $lengths[rle((ifelse(PTRAN > 0, TRAN / PTRAN, 1)) < 0.8)$values]), collapse = " "),
                         Defsum_TRATIO_lower80 = round(sum((1 - (ifelse(PTRAN > 0, TRAN / PTRAN, 1) / 0.8)) * (ifelse(PTRAN > 0, TRAN / PTRAN, 1) < 0.8)),3)
  ),
  by = list(YR, MO)]
  return(dat_month)
}

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

