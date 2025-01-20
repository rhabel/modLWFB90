#' Soil-list creation from STOKA data
#'
#' This function transforms data as currently stored in the LEITPROFIL-database and returns a list of soil data frames as recognised by \code{LWFBrook90R}. The list is further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{run_multisite_LWFB90}}.
#'
#' @param df A data frame containing the columns \code{ID} and \code{ID_custom} as distinct assignment from the initial \code{df.ids} dataframe and the column \code{RST_F}, which is created in \code{\link{fnc_get_soil}} through a spatial join of the coordinates with the STOK-BW shapefile (or, at this stage, the shapefile of test areas).
#' @param df.LEIT a data frame containing LEITPROFILE. At this stage of development, the comprehensive BW-wide database is not complete yet, so the test area needs to be assigned here. However, this is done automatically in  \code{\link{fnc_get_soil}}.
#' @param PTF_to_use which PTF will later be used in \code{\link{fnc_get_soil}} has an impact on the setting of oc.pct, so this information is passed down from \code{\link{fnc_get_soil}} here.
#' @param limit_bodtief max soil depth, default is \code{NA} and uses max soil depth as defined in \code{df.LEIT}. If not \code{NA} soil-dfs are created down to the depth specified here as depth in \code{m}, negative. Might be used to give room for different \code{maxrootdepth} - settings in \link{fnc_get_params}. In this case, soil depth may be reduced significantly.
#' @param parallel_processing shall the process be run parallely
#' @param maxcores if \code{parallel_processing = T}, on how many cores shall the process be run parallely
#'
#' @return Returns a list of soil data frames.

fnc_soil_stok <- function(df,
                          df.LEIT,
                          PTF_to_use,
                          limit_bodtief,
                          parallel_processing = F,
                          maxcores = parallel::detectCores()-1){


  if(parallel_processing){
    # get Leitprofile info through parallel processing
    cl <- parallel::makeCluster(maxcores)
    doParallel::registerDoParallel(cl)

    ls.soil.par <- foreach::foreach(i = 1:nrow(df),
                                    .packages = c("dplyr", "modLWFB90")) %dopar% {
                                      tryCatch({

                                        df <- as.data.frame(df)

                                        df.tmp <- left_join(df[i,], df.LEIT, by = "RST_F") %>%
                                          dplyr::select(ID, ID_custom, BODENTY, slope, aspect,
                                                        Horizont, HNr, Tiefe_OG, Tiefe_UG,
                                                        Sand, Schluff, Ton, Skelett, TRD, SOC, humus) %>%
                                          setNames(c("ID", "ID_custom", "BODENTYP", "slope", "aspect",
                                                     "horizont", "mat", "upper", "lower",
                                                     "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humus"))

                                        df.tmp[df.tmp == -9999] <- NA

                                        # remove roots from Sd/Gr-Horizons
                                        noroots <- which(stringr::str_detect(df.tmp$horizont,"Sd|Srd|Gor|Gr"))
                                        if(length(noroots)>0){
                                          df.tmp$dpth_ini <- df.tmp$lower[min(noroots)-1]
                                        }else{
                                          df.tmp$dpth_ini <- max(df.tmp$lower)
                                        }

                                        # Tiefendiskretisierung, limit if wanted
                                        if(!all(is.na(df.tmp[,c("mat", "upper", "lower")]))){

                                          df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                                              limit_bodtief = limit_bodtief)

                                        }

                                        # translate humusform to humus-cm
                                        df.tmp <- df.tmp %>%
                                          dplyr::mutate(
                                            oc.pct = case_when((
                                              is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                              (is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use %in% c("HYPRES", "WESSOLEK") ~ 0.1,
                                              T ~ oc.pct),
                                            upper = upper/-100,
                                            lower = lower/-100,
                                            gravel = gravel / 100) %>%
                                          dplyr::select(ID, ID_custom, BODENTYP, slope, aspect, horizont, mat, everything())

                                      },
                                      error = function(cond){

                                        out <- data.frame("ID" = df$ID[i],
                                                          "error" = conditionMessage(cond))
                                        return(out)

                                      })
                                    }

    parallel::stopCluster(cl)
  }else{

    if(class(df)[1] == "sf"){
      df <- df %>% st_drop_geometry()

    }

    ls.soil.par <- list()

    for(i in 1:nrow(df)){
      df.out <- tryCatch(

        {
          df.tmp <- left_join(df[i,], df.LEIT, by = "RST_F") %>%
            dplyr::select(ID, ID_custom, BODENTY, slope, aspect,
                          Horizont, HNr, Tiefe_OG, Tiefe_UG,
                          Sand, Schluff, Ton, Skelett, TRD, SOC, humus) %>%
            setNames(c("ID", "ID_custom", "BODENTYP", "slope", "aspect",
                       "horizont", "mat", "upper", "lower",
                       "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humus"))

          df.tmp[df.tmp == -9999] <- NA

          # remove roots from Sd/Gr-Horizons
          noroots <- which(stringr::str_detect(df.tmp$horizont,"Sd|Srd|Gor|Gr"))
          if(length(noroots)>0){
            df.tmp$dpth_ini <- df.tmp$lower[min(noroots)-1]
          }else{
            df.tmp$dpth_ini <- max(df.tmp$lower)
          }

          # Tiefendiskretisierung, limit if wanted
          if(!all(is.na(df.tmp[,c("mat", "upper", "lower")]))){

            df.tmp <- modLWFB90::fnc_depth_disc(df = df.tmp,
                                                limit_bodtief = limit_bodtief)

          }

          # translate humusform to humus-cm
          df.tmp <- df.tmp %>%
            dplyr::mutate(
              oc.pct = case_when((
                is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use == "PTFPUH2" ~ 0.5,
                (is.na(oc.pct)|(oc.pct < 0)) & PTF_to_use %in% c("HYPRES", "WESSOLEK") ~ 0.1,
                T ~ oc.pct),
              upper = upper/-100,
              lower = lower/-100,
              gravel = gravel / 100) %>%
            dplyr::select(ID, ID_custom, BODENTYP, slope, aspect, horizont, mat, everything())

        },

        error = function(cond){

          df.out <- data.frame("ID_custom" = as.character(as.data.frame(x)$ID_custom),
                            "error" = conditionMessage(cond))
          return(df.out)

        }
      )
      ls.soil.par[[i]] <- df.out
    }
  }

  names(ls.soil.par) <- unlist(lapply(ls.soil.par, function(x) unique(x$ID_custom)))

  # set NULL to missing data
  ls.soil.par[which(unlist(lapply(ls.soil.par, function(x) nrow(x))) == 1)] <- NULL

  return(ls.soil.par)
}
