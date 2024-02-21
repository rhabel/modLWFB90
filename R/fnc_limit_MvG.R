#' Restriction of Mualem-vanGenuchten Values
#'
#' The function fnc_PTF creates Mualem-vanGenuchten parameters from physical soil properties. Since some of these PTFs may extrapolate to values that are unrealistic, this function, if activated through \code{limit_mvG} in \code{fnc_create_boden}(), limits the parameters to parameter ranges as described in "U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb".
#'
#'
#' @param df a data frame containing soil properties including MvG-alpha (column \code{alpha}), MvG-alpha (column \code{alpha}), stone vol_% (column \code{gravel}), MvG-npar (column \code{npar}), saturated conductivity (column \code{ksat}), and tortuousity (column \code{tort}).
#'
#' @details
#' ranges are as follows:
#' \itemize{
#' \item alpha - between 0.1 and 500 m-1
#' \item gravel - max 95% if gravel > 95%
#' \item npar - npar between 2 and 1.1 (the latter reducing error: 'FWETK failed to determine wetness at KF')
#' \item ksat - min 0.01 mm/d if ksat < 0.01
#' \item tort - between -3 and 6
#' }
#'
#'
#' @return returns the same data frame, just with parameters limited as described in the function.
#'
#'
fnc_limit <- function(df){
  if("alpha" %in% colnames(df)){
    df <- df %>%
      mutate(alpha = case_when(alpha < 0.1 ~ 0.1,
                               alpha > 500 ~ 500,
                               T ~ alpha))
  }
  if("gravel" %in% colnames(df)){
    df <- df %>%
      mutate(gravel = case_when(gravel > 0.95 ~ 0.95,
                                T ~ gravel))
  }
  if("npar" %in% colnames(df)){
    df <- df %>%
      mutate(npar = case_when(npar > 2 ~ 2,
                              npar < 1.1 ~ 1.1,
                              T ~ npar))
  }
  if("ksat" %in% colnames(df)){
    df <- df %>%
      mutate(ksat = case_when(ksat < 2 ~ 2,
                              T ~ ksat))
  }
  if("tort" %in% colnames(df)){
    df <- df %>%
      mutate( tort = case_when(tort < -3 ~ -3,
                               tort > 6 ~ 6,
                               T ~ tort))
  }
  return(df)
}
