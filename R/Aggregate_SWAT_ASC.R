#' Non exportet aggregate function over profile
#'
#' ...
#'
#' @param SWATi swati from swatday.asc
#' @param soil soil.df
#'
#' @return Returns the desired output to the database directly.
#'
#'
#' @import data.table
#' @export


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
