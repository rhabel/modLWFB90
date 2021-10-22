#' Soil-list creation from STOKA data
#'
#' This function transforms data as currently stored in the LEITPROFIL-database and returns a list of soil data frames as recognised by \code{LWFBrook90R}. The list is further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{msiterun}}.
#'
#' @param df A data frame containing the columns \code{ID} and \code{ID_custom} as distinct assignment from the initial \code{df.ids} dataframe and the column \code{RST_F}, which is created in \code{\link{fnc_get_soil}} through a spatial join of the coordinates with the STOK-BW shapefile (or, at this stage, the shapefile of test areas).
#' @param df.LEIT a data frame containing LEITPROFILE. At this stage of development, the comprehensive BW-wide database is not complete yet, so the test area needs to be assigned here. However, this is done automatically in  \code{\link{fnc_get_soil}}.
#' @param PTF_to_use which PTF will later be used in \code{\link{fnc_get_soil}} has an impact on the setting of oc.pct, so this information is passed down from \code{\link{fnc_get_soil}} here.
#' @param dgm df.dgm gets created in \code{\link{fnc_get_soil}} and is passed here to avoid complications with potential df.dgms in the global environment.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}. If not \code{NA} soil-dfs are created down to the depth specified here as depth in \code{m}, negative. Might be used to give room for different \code{maxrootdepth} - settings in \link{fnc_get_params}. In this case, soil depth may be reduced significantly.
#'
#' @return Returns a list of soil data frames.

fnc_soil_stok <- function(df,
                          df.LEIT,
                          PTF_to_use,
                          dgm,
                          limit_bodtief,
                          incl_GEOLA){


  # get Leitprofile info through parallel processing
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)

  ls.soil.par <- foreach::foreach(i = 1:nrow(df),
                   .packages = c("dplyr", "modLWFB90")) %dopar% {
                    tryCatch({
                       df.tmp <- df.LEIT %>%
                         dplyr::filter(RST_F == df$RST_F[i]) %>%
                         dplyr::mutate("ID" = df$ID[i],
                                       "ID_custom" = as.character(df$ID_custom[i])) %>%
                         dplyr::mutate(TRD = round(as.numeric(TRD), 3)) %>%

                         dplyr::select(ID, ID_custom, LAGENUM, TIEFE_OG, TIEFE_UG, SAND, SCHLUFF, TON, SKELETT, TRD, SOC, humusform) %>%
                         setNames(c("ID", "ID_custom", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humusform")) %>%

                         dplyr::mutate_at(vars(-all_of(c("ID_custom", "humusform"))), as.numeric)

                       if(incl_GEOLA){
                         df.tmp[nrow(df.tmp), "lower"] <-  as.numeric(max(df$GRUND_C, max(df.tmp$lower)))
                       }

                       # Tiefendiskretisierung, limit if wanted
                       if(is.na(limit_bodtief) == TRUE){

                         df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                             limit_bodtief = NA)

                       }else{

                         df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                             limit_bodtief = limit_bodtief)

                       }

                       # translate humusform to humus-cm
                       df.tmp <- df.tmp %>%
                         dplyr::mutate(oc.pct = case_when((is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                                          (is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use %in% c("HYPRES", "WESSOLEK") ~ 0.1,
                                                          T ~ oc.pct),
                                       humus = case_when(humusform == "Mull" ~ 0.03,
                                                         humusform == "Mullmoder" ~ 0.067,
                                                         humusform == "Moder" ~ 0.045,
                                                         humusform == "Rohhumusartiger Moder" ~ 0.06,
                                                         humusform == "Rohhumus" ~ 0.07,
                                                         T ~ 0),
                                       upper = upper/-100,
                                       lower = lower/-100,
                                       gravel = gravel / 100)

                       # add and prepare
                       df.tmp <- df.tmp %>%
                         dplyr::mutate(nl = 1:nrow(df.tmp)) %>%
                         dplyr::left_join(dgm, by = "ID") %>%
                         dplyr::select(ID, ID_custom, mat, nl, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus) %>%
                         dplyr::mutate(ID_custom = as.character(ID_custom))

                     },
                     error = function(cond){
                       out <- data.frame("ID" = df$ID[i],
                                         "ID_custom" = as.character(df$ID_custom[i]))
                       return(out)
                       # cond
                     })
                   }

  parallel::stopCluster(cl)

  # name acc. to ordered IDs
  names(ls.soil.par) <- unlist(lapply(ls.soil.par, function(x) unique(x$ID)))
  # sort
  ls.soil.par = ls.soil.par[stringr::str_sort(names(ls.soil.par), numeric = T)]
  # rename acc. to ID_custom
  names(ls.soil.par) <- unlist(lapply(ls.soil.par, function(x) unique(x$ID_custom)))
  # set NULL to missing data
  ls.soil.par[which(unlist(lapply(ls.soil.par, function(x) nrow(x))) == 1)] <- NULL


  return(ls.soil.par)
}
