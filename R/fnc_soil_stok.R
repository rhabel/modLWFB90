#' Soil-list creation from STOKA data
#'
#' This function transforms data as currently stored in the LEITPROFIL-database and returns a list of soil data frames as recognised by \code{LWFBrook90R}. The list is further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{run_multisite_LWFB90}}.
#'
#' @param df A data frame containing the columns \code{ID} and \code{ID_custom} as distinct assignment from the initial \code{df.ids} dataframe and the column \code{RST_F}, which is created in \code{\link{fnc_get_soil}} through a spatial join of the coordinates with the STOK-BW shapefile (or, at this stage, the shapefile of test areas).
#' @param df.LEIT a data frame containing LEITPROFILE. At this stage of development, the comprehensive BW-wide database is not complete yet, so the test area needs to be assigned here. However, this is done automatically in  \code{\link{fnc_get_soil}}.
#' @param PTF_to_use which PTF will later be used in \code{\link{fnc_get_soil}} has an impact on the setting of oc.pct, so this information is passed down from \code{\link{fnc_get_soil}} here.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}. If not \code{NA} soil-dfs are created down to the depth specified here as depth in \code{m}, negative. Might be used to give room for different \code{maxrootdepth} - settings in \link{fnc_get_params}. In this case, soil depth may be reduced significantly.
#'
#' @return Returns a list of soil data frames.

fnc_soil_stok <- function(df,
                          df.LEIT,
                          PTF_to_use,
                          limit_bodtief,
                          incl_GEOLA,
                          parallel_processing = F){


  if(parallel_processing){
    # get Leitprofile info through parallel processing
    cl <- parallel::makeCluster(parallel::detectCores())
    doParallel::registerDoParallel(cl)

    ls.soil.par <- foreach::foreach(i = 1:nrow(df),
                                    .packages = c("dplyr", "modLWFB90")) %dopar% {
                                      tryCatch({

                                        df.tmp <- df.LEIT %>%
                                          dplyr::filter(RST_F == df$RST_F[i]) %>%
                                          dplyr::mutate("ID" = df$ID[i],
                                                        "ID_custom" = as.character(df$ID_custom[i]),
                                                        "humus" = df$humus[i]) %>%

                                          #dplyr::select(ID, ID_custom, Horizont, HNr, TIEFE_OG, TIEFE_UG, SAND, SCHLUFF, TON, SKELETT, TRD, SOC, humusform) %>%
                                          dplyr::select(ID, ID_custom, Horizont, HNr, Tiefe_OG, Tiefe_UG, Sand, Schluff, Ton, Skelett, TRD, SOC, humus) %>%
                                          setNames(c("ID", "ID_custom", "horizont", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humus"))# %>%

                                        # dplyr::mutate_at(vars(-all_of(c("ID_custom", "horizont"))), as.numeric) %>%
                                        # dplyr::mutate(horizont = stringr::str_sub(stringr::str_replace_all(horizont, " 1| 2| 3", ""), -3, -1))
                                        df.tmp[df.tmp == -9999] <- NA

                                        # remove roots from Sd/Gr-Horizons
                                        noroots <- which(stringr::str_detect(df.tmp$horizont,"Sd|Srd|Gor|Gr"))
                                        if(length(noroots)>0){rootslim_soil <- df.tmp$lower[min(noroots)-1]}else{rootslim_soil <- max(df.tmp$lower)}

                                        df.tmp$dpth_ini <- rootslim_soil
                                        df.tmp$BODENTYP <- "unknown"

                                        if(incl_GEOLA){

                                          if(!is.na(df$BODENTY[i])){
                                            df.tmp$BODENTYP <-  df$BODENTY[i]
                                          }

                                          # if no Sd/Gr Horizon, take deepest depth GEOLA/STOK as roots and profile limit

                                          if(length(noroots) == 0 & !is.na(df$GRUND_C[i])){
                                            whichmax <- as.numeric(max(df$GRUND_C[i], rootslim_soil))
                                            df.tmp[nrow(df.tmp), "lower"] <-  whichmax
                                            df.tmp$dpth_ini <- whichmax

                                          }

                                        }

                                        # Tiefendiskretisierung, limit if wanted
                                        if(!all(is.na(df.tmp[,c("mat", "upper", "lower")]))){
                                          if(is.na(limit_bodtief) == TRUE){

                                            if(incl_GEOLA){
                                              df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                  limit_bodtief = ifelse(df$BODENTY[i] == "Gleye/Auenboeden",
                                                                                                         -3,NA))
                                            }else{
                                              df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                  limit_bodtief = NA)
                                            }


                                          }else{

                                            df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                limit_bodtief = limit_bodtief)

                                          }
                                        }


                                        # translate humusform to humus-cm
                                        df.tmp <- df.tmp %>%
                                          dplyr::mutate(oc.pct = case_when((is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                                                           (is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use %in% c("HYPRES", "WESSOLEK") ~ 0.1,
                                                                           T ~ oc.pct),
                                                        upper = upper/-100,
                                                        lower = lower/-100,
                                                        gravel = gravel / 100)

                                        # add and prepare
                                        df.tmp <- df.tmp %>%
                                          dplyr::mutate(nl = 1:nrow(df.tmp)) %>%
                                          dplyr::left_join(df[i,c("ID", "aspect", "slope")]) %>%
                                          dplyr::select(-horizont) %>%
                                          dplyr::select(ID, ID_custom, mat, nl, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus, everything()) %>%
                                          dplyr::mutate(ID_custom = as.character(ID_custom))

                                      },
                                      error = function(cond){
                                        out <- data.frame("ID" = df$ID[i],
                                                          "ID_custom" = as.character(df$ID_custom[i]))
                                        return(out)

                                      })
                                    }

    parallel::stopCluster(cl)
  }else{
    ls.soil.par <- apply(df, 1, FUN = function(x){

                                    out <- tryCatch(

                                      {
                                        df.tmp <- df.LEIT %>%
                                          dplyr::filter(RST_F == x[["RST_F"]]) %>%
                                          dplyr::mutate("ID" = x[["ID"]],
                                                        "ID_custom" = as.character(x[["ID_custom"]]),
                                                        "humus" = x[["humus"]]) %>%

                                          dplyr::select(ID, ID_custom, Horizont, HNr, Tiefe_OG, Tiefe_UG, Sand, Schluff, Ton, Skelett, TRD, SOC, humus) %>%
                                          setNames(c("ID", "ID_custom", "horizont", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humus"))

                                        df.tmp[df.tmp == -9999] <- NA

                                        # remove roots from Sd/Gr-Horizons
                                        noroots <- which(stringr::str_detect(df.tmp$horizont,"Sd|Srd|Gor|Gr"))
                                        if(length(noroots)>0){rootslim_soil <- df.tmp$lower[min(noroots)-1]}else{rootslim_soil <- max(df.tmp$lower)}

                                        df.tmp$dpth_ini <- rootslim_soil
                                        df.tmp$BODENTYP <- "unknown"

                                        if(incl_GEOLA){

                                          if(!is.na(x[["BODENTY"]])){
                                            df.tmp$BODENTYP <-  x[["BODENTY"]]
                                          }

                                          # if no Sd/Gr Horizon, take deepest depth GEOLA/STOK as roots and profile limit

                                          if(length(noroots) == 0 & !is.na(x[["GRUND_C"]])){
                                            whichmax <- as.numeric(max(x[["GRUND_C"]], rootslim_soil))
                                            df.tmp[nrow(df.tmp), "lower"] <-  whichmax
                                            df.tmp$dpth_ini <- whichmax

                                          }

                                        }

                                        # Tiefendiskretisierung, limit if wanted
                                        if(!all(is.na(df.tmp[,c("mat", "upper", "lower")]))){
                                          if(is.na(limit_bodtief) == TRUE){

                                            if(incl_GEOLA){
                                              df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                  limit_bodtief = ifelse( x[["BODENTY"]] == "Gleye/Auenboeden",
                                                                                                         -3,NA))
                                            }else{
                                              df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                  limit_bodtief = NA)
                                            }


                                          }else{

                                            df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                                limit_bodtief = limit_bodtief)

                                          }
                                        }


                                        # translate humusform to humus-cm
                                        df.tmp <- df.tmp %>%
                                          dplyr::mutate(oc.pct = case_when((is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                                                           (is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use %in% c("HYPRES", "WESSOLEK") ~ 0.1,
                                                                           T ~ oc.pct),
                                                        upper = upper/-100,
                                                        lower = lower/-100,
                                                        gravel = gravel / 100)

                                        # add and prepare
                                        df.tmp <- df.tmp %>%
                                          dplyr::mutate(nl = 1:nrow(df.tmp),
                                                        slope = as.numeric(x[["slope"]]),
                                                        aspect = as.numeric(x[["aspect"]]),
                                                        ID = as.numeric(ID),
                                                        humus = as.numeric(humus)) %>%
                                          dplyr::select(-horizont) %>%
                                          dplyr::select(ID, ID_custom, mat, nl, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus, everything()) %>%
                                          dplyr::mutate(ID_custom = as.character(ID_custom))

                                      },
                                      error = function(cond){
                                        out <- data.frame("ID" = x[["ID"]],
                                                          "ID_custom" = as.character(x[["ID_custom"]]))
                                        return(out)

                                      }
                                    )
      })
  }


  names(ls.soil.par) <- unlist(lapply(ls.soil.par, function(x) unique(x$ID_custom)))
  # set NULL to missing data
  ls.soil.par[which(unlist(lapply(ls.soil.par, function(x) nrow(x))) == 1)] <- NULL

  return(ls.soil.par)
}
