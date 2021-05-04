#' Calculation of the plant available water content in % and mm
#'
#'
#'This function quickly adds nFK in % and mm to a list of soil data frames
#'
#' @param df is the list of soil properties created by \code{\link{fnc_get_soil}}
#'
#' @return returns the same list of soil properties with columns containing AWC-infos added
#' @export

fnc_add_nFK <- function(df){
  # check if all necessary columns are there:
  missingcol <- c("ths", "thr", "alpha", "npar",
                  "upper", "lower", "gravel")[!c("ths", "thr", "alpha", "npar",
                                                 "upper", "lower", "gravel") %in% names(df)]
  if (length(missingcol) > 0){
    cat(missingcol, "is missing in colnames(df)", "\n")
    stop()
  }

  soil <- as.data.table(df)
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

  soil <- soil[, c("FK","PWP", "nFK","FK.mm","PWP.mm", "nFK.mm") := lapply(.SD, round, 3),
               .SDcols = c("FK","PWP", "nFK","FK.mm","PWP.mm", "nFK.mm")]

  soil <- soil[,c("nFK_WR", "nFK_WR_nohum",
                  "nFK_100", "nFK_100_nohum") := list(
                    nFK_WR =  round(sum(nFK.mm * (rootden > 0)),1),
                    nFK_WR_nohum =  round(sum(nFK.mm * (upper <= 0 & rootden > 0)),1),
                    nFK_100 =  round(sum(nFK.mm * (lower >= -1.0)),1),
                    nFK_100_nohum =  round(sum(nFK.mm * (upper <= 0 & lower >= -1.0)),1))]

  sscdata <- as.data.frame(soil)[c("sand", "silt", "clay")]
  colnames(sscdata) <- c("SAND", "SILT", "CLAY")
  sscdata <- sscdata[rowSums(sscdata)!=0, ]
  sscdata$nl <- as.numeric(rownames(sscdata))

  texture <- soiltexture::TT.points.in.classes(tri.data = as.data.frame(sscdata), class.sys = "DE.BK94.TT", text.tol = 0.01)
  sscdata$texture <- colnames(texture)[apply(texture,1,which.max)]

  soil <- dplyr::left_join(soil, sscdata[c("nl", "texture")], by = "nl")

  # soil <- tibble::as_tibble(soil)
  return(soil)
}
