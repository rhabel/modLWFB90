#' Fine-Root distribution
#'
#' This function adds fine roots to the soil dataframes.
#'
#' @param df a data frame containing soil properties including MvG-alpha (column \code{alpha}), MvG-npar (column \code{npar}), theta_s (column \code{ths}), theta_r (column \code{thr}), and organic carbon in pct (column \code{oc.pct}.
#' @param rootsmethod name of the method for fine roots. Possible options are \code{hartmann},\code{betamodel}, \code{table}, \code{constant}, and \code{linear}. Default is \code{betamodel}.
#' @param humus_roots decides whether humus layers get roots too. Default is \code{TRUE}. If \code{FALSE}, humus layer gets a rootden-value of \code{0}. If \code{TRUE} and \code{rel_hum_val = NA}, humus layer gets half the amount of roots as highest soil layer. If \code{rel_hum_val} is a value, humus layer gets this value
#' @param rel_hum_val roots in humuslayer as fraction of roots in soil. Default is \code{NA}
#' @param ... additional input arguments that are passed on to \code{make_rootden}, see \code{\link[LWFBrook90R]{make_rootden}}.
#'
#' @return Returns the same data frame, but adds a rootden-column that is recognised by LWFBrook90R.
#' @references
#' Hartmann, P., Buberl, H., Puhlmann, H., Schäffer, J., Trefz-Malcher, G., Zirlewagen, D., von Wilpert, K. (2016): Waldböden Südwestdeutschlands - Ergebnisse der Bodenzustandserhebungen im Wald von 1989 – 1992 und 2006 – 2008. Verlag Kessel, Remagen-Oberwinter, 328 S
#'
#' @export
fnc_roots <- function(df,
                      rootsmethod = "betamodel",
                      humus_roots = T,
                      rel_hum_val = NA,
                      ...){

  if(rootsmethod == "hartmann"){
    df <- df %>%
      dplyr::mutate(nfk = (fnc_MvG.swc(63,alpha/100, npar, ths, thr) - fnc_MvG.swc(15000,alpha/100, npar, ths, thr)),
                    hum.ka5 = case_when(oc.pct *1.72 < 0.1 ~ 1,
                                        oc.pct *1.72 >= 0.1 & oc.pct *1.72 < 1 ~ 2,
                                        oc.pct *1.72 >= 1 & oc.pct *1.72 < 2 ~ 3,
                                        oc.pct *1.72 >= 2 & oc.pct *1.72 < 5 ~ 4,
                                        oc.pct *1.72 >= 5 & oc.pct *1.72 < 10 ~ 5,
                                        oc.pct *1.72 >= 10 & oc.pct *1.72 < 15 ~ 6,
                                        oc.pct *1.72 >= 15 ~ 7),
                    i.upper = upper *-100,
                    i.lower = lower *-100)

    df <- df %>%
      dplyr::mutate(fwd_brt = sapply(1:nrow(df),
                                     function(i) mean(11.63 - 0.084*seq(i.upper[i]+1, i.lower[i]) + 3.22*hum.ka5[i] - 3.42*bd[i] + 0.108*slope[i] + 0.095*nfk[i]*100)))

    df <- df %>%
      dplyr::mutate(fwd_brt = case_when(i.upper < 0 ~ NA_real_,
                                        T ~ fwd_brt))
    df <- df %>%
      dplyr::mutate(fwd_brt = case_when(i.upper < 0 & humus_roots == T & is.na(rel_hum_val) ~ max(fwd_brt, na.rm = T)/2,
                                        i.upper < 0 & humus_roots == F ~ 0,
                                        T ~ fwd_brt)) %>%
      dplyr::select(-nfk, -hum.ka5, -i.upper, -i.lower) %>%
      dplyr::rename(rootden = fwd_brt) %>%
      dplyr::mutate(rootden = ifelse(rootden < 2, 0, rootden))

    sumroots <- sum(df$rootden)
    df$rootden <- round(df$rootden/sumroots, 5)

    if(df$upper[1] > 0 & humus_roots == T & !is.na(rel_hum_val)){
      df$rootden[1] <- rel_hum_val
      sumroots <- sum(df$rootden)
      df$rootden <- round(df$rootden/sumroots, 5)
    }

    return(df)
  }else{
    rootden <- make_rootden_adj(soilnodes = df$lower,
                                method = rootsmethod,
                                ...)

    # rel.values
    sumroots <- sum(rootden)
    rootden <- round(rootden/sumroots, 5)

    # humus treatment:
    if(df$upper[1] > 0){
      if(humus_roots == T){
        if(is.na(rel_hum_val)){

          # humus gets half of top soil horizon
          df$rootden <- c(round(max(rootden)/2,5),
                          rootden)
          # again  - sum of rootden to 1
          sumroots <- sum(df$rootden)
          df$rootden <- round(df$rootden/sumroots, 5)

        }else{
          # humus gets rel_hum_val
          df$rootden <- c(rel_hum_val,
                          rootden)
          # again  - sum of rootden to 1
          sumroots <- sum(df$rootden)
          df$rootden <- round(df$rootden/sumroots, 5)
        }
      }

    }else{
      df$rootden <- c(rootden, tail(rootden, 1))
    }
    return(df)
  }

}

make_rootden_adj <- function(soilnodes,
                             roots_max_adj = min(soilnodes),
                             roots_max = min(soilnodes),
                             method = "betamodel",
                             beta = 0.97,
                             rootdat = NULL) {

  maxrootdepth <- roots_max_adj

  # roots_max <- roots_max+1

  if (method == "betamodel") {

    # only positive d-values allowed in beta-model:
    maxrootdepth <- maxrootdepth * (-100)
    soilnodes <- soilnodes * (-100)

    # replace first element greater maxrootdepth with maxrootdepth
    soilnodes_maxrtdep <- soilnodes

    if(max(soilnodes) > maxrootdepth){
      soilnodes_maxrtdep[which.max(soilnodes >= maxrootdepth)] <- maxrootdepth
    }

    # shift downwards to account for negative values in soilnodes (humus topsoil layers)
    if (min(soilnodes_maxrtdep) < 0) {
      maxrootdepth <- maxrootdepth - min(soilnodes_maxrtdep)
      soilnodes_maxrtdep <- soilnodes_maxrtdep - min(soilnodes_maxrtdep)
    }

    # cumulative density
    RLenDcum <- 1 - (beta ^ soilnodes_maxrtdep)

    # density
    rootden <- diff(RLenDcum)/diff(soilnodes) * 1000# important to use soilnodes here, so rootden is reduced if the lowest layer is only partially within maxrootdpeth
    rootden[which(soilnodes_maxrtdep > maxrootdepth) - 1] <- 0

  }

  if (method == "constant") {
    rootden <- rep(1,length(soilnodes)-1)
    rootden[which(soilnodes[1:length(soilnodes)-1] <= maxrootdepth)] <- 0
  }

  if (method == "linear") {
    RelDenFun <- stats::approxfun(x = c(max(soilnodes),maxrootdepth), y = c(1,0), method = "linear",rule = 1:2, yleft = 0)
    soilnodes[which.max(soilnodes <= maxrootdepth)] <- maxrootdepth
    midpoints <- soilnodes[1:length(soilnodes)-1] + diff(soilnodes)/2
    rootden <- RelDenFun(midpoints)
  }

  if (method == "table") {

    # to pass CRAN check notes
    upper <- NULL; lower <- NULL; i.upper <- NULL; i.lower <- NULL;
    rootmass <- NULL; rthick <- NULL; thick_ol <- NULL;

    # distributes 'measured' relative root densities to the soil layers, preserving total root mass

    stopifnot(all(c("upper", "lower", "rootden") %in% tolower(names(rootdat))))
    names(rootdat) <- tolower(names(rootdat))

    # create data.tables for overlap join
    rootdat <- data.table::data.table(rootdat, key = c("lower", "upper"))
    rootdat[, rthick := (upper - lower)]
    rootdat[, rootmass := (rootden*rthick)]

    slayers <- data.table::data.table(upper = soilnodes[1:length(soilnodes)-1],
                                      lower = soilnodes[2:length(soilnodes)])

    # overlap-join
    rootdat <- data.table::foverlaps(slayers,rootdat, type = "any")

    # derive overlap-thickness for soil layers
    rootdat[, thick_ol := (ifelse(i.upper < upper,i.upper,upper) -
                             ifelse(i.lower < lower & i.upper > lower,lower,
                                    ifelse(i.upper < lower,0,i.lower)) ) * (i.upper > lower & i.lower < upper)]

    # sum up rootmass proportional to overlapping thickness
    out <- rootdat[, list(i.rootmass = sum(rootmass*thick_ol/rthick)), by = list(i.upper ,i.lower)]

    # convert rootmass back to root density
    out$i.rden <- with(out, ifelse(!is.na(i.rootmass),i.rootmass / (i.upper - i.lower), 0) )

    rootden <- out$i.rden

  }
  return(rootden)
}

