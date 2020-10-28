#' Fine-Root distribution
#'
#' This function adds fine roots to the soil dataframes.
#'
#' @param df a data frame containing soil properties including MvG-alpha (column \code{alpha}), MvG-npar (column \code{npar}), theta_s (column \code{ths}), theta_r (column \code{thr}), and organic carbon in pct (column \code{oc.pct}.
#' @param rootsmethod name of the method for fine roots. Possible options are \code{hartmann},\code{betamodel}, \code{table}, \code{constant}, and \code{linear}. Default is \code{betamodel}.
#' @param humus_roots decides whether humus layers get roots too. Default is \code{TRUE}. If \code{TRUE}, humus layer gets the same amount of roots as highest soil layer, else \code{0}.
#' @param ... additional input arguments that are passed on to \code{MakeRelRootDens}, see \code{\link[LWFBrook90R]{MakeRelRootDens}}.
#'
#' @return Returns the same data frame, but adds a rootden-column that is recognised by LWFBrook90R.
#' @references
#' Hartmann, P., Buberl, H., Puhlmann, H., Schäffer, J., Trefz-Malcher, G., Zirlewagen, D., von Wilpert, K. (2016): Waldböden Südwestdeutschlands - Ergebnisse der Bodenzustandserhebungen im Wald von 1989 – 1992 und 2006 – 2008. Verlag Kessel, Remagen-Oberwinter, 328 S
#'
#' @export
fnc_roots <- function(df,
                      rootsmethod = "betamodel",
                      humus_roots = T,
                      ...){

  rootsmethod <- match.arg(rootsmethod, choices = c("hartmann", "betamodel", "table", "constant", "linear"))

  if(rootsmethod == "hartmann"){
    df <- df %>%
      dplyr::mutate(nfk = (fnc_MvG.swc(63,alpha/100, npar, ths, thr) - fnc_MvG.swc(15000,alpha/100, npar, ths, thr)),
                    hum.ka5 = case_when(oc.pct *1.72 < 0.1 ~ 1,
                                        oc.pct *1.72 >= 0.1 & oc.pct *1.72 < 1 ~ 2,
                                        oc.pct *1.72 >= 1 & oc.pct *1.72 < 2 ~ 3,
                                        oc.pct *1.72 >= 2 & oc.pct *1.72 < 5 ~ 4,
                                        oc.pct *1.72 >= 5 & oc.pct *1.72 < 10 ~ 5,
                                        oc.pct *1.72 >= 10 & oc.pct *1.72 < 15 ~ 6,
                                        oc.pct *1.72 >= 30 & oc.pct *1.72 < 30 ~ 7))

    df <- df %>%
      dplyr::mutate(fwd_brt = sapply(1:nrow(df),
                                     function(i) mean(11.63 - 0.084*seq(upper[i]+1, lower[i]) + 3.22*hum.ka5[i] - 3.42*bd[i] + 0.108*slope[i] + 0.095*nfk[i]*100))) %>%
      dplyr::mutate(fwd_brt = case_when(is.na(fwd_brt) & humus_roots == T ~ max(fwd_brt, na.rm = T),
                                        is.na(fwd_brt) & humus_roots == F ~ 0,
                                        T ~ fwd_brt)) %>%
      dplyr::select(-nfk, -hum.ka5) %>%
      dplyr::rename(rootden = fwd_brt)
    return(df)
  }else{
    rootden <- LWFBrook90R::MakeRelRootDens(soilnodes = df$lower[-1], method = rootsmethod, ...)
    df$rootden <- c(ifelse(humus_roots == T, max(rootden), 0),
                    rootden)
    return(df)
  }

}


