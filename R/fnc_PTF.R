#' PTF application and humus layer creation
#'
#' This function takes the data frame of soil physics data and creates the hydraulic parameters. It further creates humus-layers using the MvG-parameters from Hammel&Kennel (2001)
#'
#' @param df data frame containing information on soil physics in the following columns:
#' \itemize{
#'   \item sand, silt, clay - soil texture in mass %
#'   \item bd - bulk density in g cm-1
#'   \item oc.pct - organic carbon in mass %
#'   \item humus - thickness of the humus-layer (repeated \code{nrow} - times. a bit clumsy, but building on existing code it was the most convenient way) \cr LWFB90 has problems if the first row is too thin and creates surface flow if it becomes saturated, which can happen if the humus layer is too thin. So, if humus is <= 2cm, it will be removed, 3 cm will be turned into 4 cm. Anything above is fine.
#' }
#' @param PTF_used PTF-options from the \code{LWFBrook90R} - package. Choices are \code{"HYPRES"}, \code{"PTFPUH2"}, or \code{"WESSOLEK"}.
#' @param limit_humus if \code{ilayer = 0} and \code{infexp = 0}, which is the default setting in \code{\link[LWFBrook90R]{set_paramLWFB90}}, water can only infiltrate in the first layer. If thin humus layers (e.g. 2 cm) are put on top of the first mineral soil layer, there's a risk of creating unrealisic amounts of surface runoff, when the water storage capacity of the thin humus layer can't take the incoming amount of daily rain. Hence, this parameter deletes humus lazers smaller or equal 2 cm, and increases humus layers of 3 cm to 4 cm. Default is \code{T}. If  \code{ilayer} and \code{infexp} are greater than 0, this issue shouldn't occur and this parameter should be set to \code{F}
#'
#' @references Hammel, K., & Kennel, M. (2001). Charakterisierung und Analyse der Wasserverfügbarkeit und des Wasserhaushalts von Waldstandorten in Bayern mit dem Simulationsmodell BROOK90. Frank.
#'
#' @return Returns a longer data.table that already includes an earlier version of ls.soils. Further processed in \code{\link{fnc_get_soil}}.
#' @export

fnc_PTF <- function(df,
                    PTF_used,
                    limit_humus){

  if("humusform" %in% colnames(df)){
    df <- df %>%
      dplyr::mutate(humus = case_when(humusform == "Mull" ~ 0.03,
                                      humusform == "Mullmoder" ~ 0.067,
                                      humusform == "Moder" ~ 0.045,
                                      humusform == "Rohhumusartiger Moder" ~ 0.06,
                                      humusform == "Rohhumus" ~ 0.07,
                                      T ~ 0)) %>%
      dplyr::select(-humusform)
  }

  if(!all(c("sand","clay", "silt") %in% names(df)) & "texture" %in% colnames(df)){
    df <- df %>%
      rowwise() %>%
      mutate(sand = fnc_make_ssc(texture)[1],
             silt = fnc_make_ssc(texture)[2],
             clay = fnc_make_ssc(texture)[3]) %>%
    ungroup() %>%
      dplyr::select(-texture)
  }


  if(limit_humus){
    # thickness limits to humus layer
    df$humus <- ifelse(unique(df$humus) <= 0.02, 0,
                       ifelse(unique(df$humus) <= 0.04, 0.04, unique(df$humus)))
  }


  #
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
      stop(paste0("\n", missingcol, "is missing in df for PTF-application of ", PTF_used))
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
      stop(paste0("\n", missingcol, "is missing in df for PTF-application of ", PTF_used))
    }


  } else if (PTF_used == "WESSOLEK") {
    if(all(c("sand","clay", "silt") %in% names(df))){
      #Texture - from package "soiltexture":
      sscdata <- setNames(df[,c("sand", "silt", "clay")], c("SAND", "SILT", "CLAY"))
      sscdata <- sscdata[complete.cases(sscdata),]

      texture <- soiltexture::TT.points.in.classes(tri.data = as.data.frame(sscdata), class.sys = "DE.BK94.TT", text.tol = 0.01)
      df$texture <- colnames(texture)[apply(texture,1,which.max)]

      #order for rbind
      df <- cbind(df, LWFBrook90R::hydpar_wessolek_tab(texture = df$texture))


    }else{
      missingcol <- c("sand", "clay", "silt")[!c("sand", "clay", "silt") %in% names(df)]
      stop(paste0("ID ", df$ID[1], "\n", missingcol, " is missing in df for PTF-application of ", PTF_used))
    }

  } else {
    stop("PTF not in possible PTF-choices")
  }
  # Humus:
  humus <- df$humus[1]


  if (humus != 0){
    # rbind humus-values
    rowtobind <- data.frame("ID" = df$ID[1],
                            "ID_custom" = as.character(df$ID_custom[1]),
                            "mat" = 0,
                            "upper" = humus,
                            "lower" = 0,
                            "ths" = 0.848,
                            "thr" = 0,
                            "alpha" = 98,
                            "npar" = 1.191,
                            "mpar" = 0.160,
                            "ksat" = 98000,
                            "tort" = 0.5)

    df <- df %>% dplyr::bind_rows(., rowtobind )
    df <- df[c(nrow(df), 1:(nrow(df)-1)),]
    df <- as.data.frame(df)

    df[1,which(is.na(df[1,]))] <- df[2,which(is.na(df[1,]))]

  }

  cols <- c("aspect", "slope", "ths", "thr", "alpha", "npar", "mpar", "ksat", "tort")
  df <- df %>%
    dplyr::mutate(nl = 1:nrow(df)) %>%
    dplyr::select(-humus) %>%
    dplyr::mutate(across(any_of(cols), round, 3))

  return(df)
}
