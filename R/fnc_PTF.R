#' PTF application and humus layer creation
#'
#' This function takes the data frame of soil physics data and creates the hydraulic parameters. It further creates humus-layers using the MvG-parameters from Hammel&Kennel (2001)
#'
#' @param df data frame containing information on soil physics in the following columns:
#' \itemize{
#'   \item sand, silt, clay - soil texture in mass %
#'   \item bd - bulk density in g cm-1
#'   \item oc.pct - organic carbon in mass %
#'   \item humus - thickness of the humus-layer (repeated \code{nrow} - times. a bit clumsy, but building on existing code it was the most convenient way)
#' }
#' @param PTF_used PTF-options from the \code{LWFBrook90R} - package. Choices are \code{"HYPRES"}, \code{"PTFPUH2"}, or \code{"WESSOLEK"}.
#'
#' @references Hammel, K., & Kennel, M. (2001). Charakterisierung und Analyse der Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell BROOK90. Frank.
#'
#' @return Returns a longer data.table that already includes an earlier version of ls.soils. Further processed in \code{\link{fnc_get_soil}}.
#' @export

fnc_PTF <- function(df, PTF_used){
  if(PTF_used == "HYPRES"){
    if(all(c("clay", "silt", "bd", "oc.pct") %in% names(df))){

      test <- data.frame("CLAY" = df$clay, "SILT" = df$silt, "SAND" = df$sand)
      transftt <- soiltexture::TT.text.transf(tri.data = test,
                                              base.css.ps.lim = c(0,2,50,2000),
                                              dat.css.ps.lim = c(0,2,63,2000),
                                              text.tol = 1)

      # which are from topsoil... (>25 cm depth)
      which.topsoil <- which(df$lower >= -0.25)

      if(max(which.topsoil) < nrow(df)){
        df <- cbind(df, rbind(LWFBrook90R::hydpar_hypres(clay = transftt$CLAY[which.topsoil],
                                                         silt = transftt$SILT[which.topsoil],
                                                         bd = df$bd[which.topsoil],
                                                         oc.pct = df$oc.pct[which.topsoil],
                                                         topsoil = T),
                              LWFBrook90R::hydpar_hypres(clay = transftt$CLAY[-which.topsoil],
                                                         silt = transftt$SILT[-which.topsoil],
                                                         bd = df$bd[-which.topsoil],
                                                         oc.pct = df$oc.pct[-which.topsoil],
                                                         topsoil = F)))
      }else{
        df <- cbind(df, LWFBrook90R::hydpar_hypres(clay = transftt$CLAY,
                                                   silt = transftt$SILT,
                                                   bd = df$bd,
                                                   oc.pct = df$oc.pct,
                                                   topsoil = T))
      }




    }else{
      missingcol <- c("clay", "silt", "bd", "oc.pct")[!c("clay", "silt", "bd", "oc.pct") %in% names(df)]
      stop(paste0(missingcol, "is missing in df for PTF-application of ", PTF_used))
    }

  } else if (PTF_used == "PTFPUH2") {
    if(all(c("sand","clay", "silt", "bd", "oc.pct") %in% names(df))){
      df <- cbind(df, LWFBrook90R::hydpar_puh2(sand = df$sand,
                                               clay = df$clay,
                                               silt = df$silt,
                                               bd = df$bd,
                                               oc.pct = df$oc.pct))
    }else{
      missingcol <- c("sand", "clay", "silt", "bd", "oc.pct")[!c("sand", "clay", "silt", "bd", "oc.pct") %in% names(df)]
      stop(paste0(missingcol, "is missing in df for PTF-application of ", PTF_used))
    }


  } else if (PTF_used == "WESSOLEK") {
    if(all(c("sand","clay", "silt") %in% names(df))){
      #Texture - from package "soiltexture":
      sscdata <- setNames(df[c("sand", "silt", "clay")], c("SAND", "SILT", "CLAY"))
      sscdata <- sscdata[complete.cases(sscdata),]

      texture <- soiltexture::TT.points.in.classes(tri.data = as.data.frame(sscdata), class.sys = "DE.BK94.TT", text.tol = 0.01)
      df$texture <- colnames(texture)[apply(texture,1,which.max)]

      #order for rbind
      df <- cbind(df, LWFBrook90R::hydpar_wessolek_tab(texture = df$texture))


    }else{
      missingcol <- c("sand", "clay", "silt")[!c("sand", "clay", "silt") %in% names(df)]
      stop(paste0(missingcol, "is missing in df for PTF-application of ", PTF_used))
    }

  } else {
    stop("PTF not in possible PTF-choices")
  }

  # same order
  df <- df %>%
    dplyr::select(ID, ID_custom, mat, nl, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus, ths, thr, alpha, npar, mpar, ksat, tort)

  # Humus:
  humus <- df$humus[1]

  if (humus != 0){
    # rbind humus-values
    df <- rbind(data.frame("ID" = df$ID[1],
                           "ID_custom" = as.character(df$ID_custom[1]),
                           "mat" = 0,
                           "nl" = 0,
                           "upper" = humus,
                           "lower" = 0,
                           "sand" = 0,
                           "silt" = 0,
                           "clay" = 0,
                           "gravel" = 0,
                           "bd" = 0,
                           "oc.pct" = 0,
                           "aspect" = df$aspect[1],
                           "slope" = df$slope[1],
                           "humus" = 0,
                           "ths" = 0.848,
                           "thr" = 0,
                           "alpha" = 98,
                           "npar" = 1.191,
                           "mpar" = 0.1603694,
                           "ksat" = 98000,
                           "tort" = 0.5),
                df)

  }

  df <- df %>%
    dplyr::mutate(nl = 1:nrow(df)) %>%
    dplyr::select(-humus)

  return(df)
}
