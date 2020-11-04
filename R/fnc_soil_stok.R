#' Soil-list creation from STOKA data
#'
#' This function transforms data as currently stored in the LEITPROFIL-database and returns a list of soil data frames as recognised by \code{LWFBrook90R}. The list is further processed in \code{\link{fnc_get_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{msiterun}}.
#'
#' @param df A data frame containing the columns \code{ID} and \code{ID_custom} as distinct assignment from the initial \code{df.ids} dataframe and the column \code{RST_F}, which is created in \code{\link{fnc_get_soil}} through a spatial join of the coordinates with the STOK-BW shapefile (or, at this stage, the shapefile of test areas).
#' @param df.LEIT a data frame containing LEITPROFILE. At this stage of development, the comprehensive BW-wide database is not complete yet, so the test area needs to be assigned here. However, this is done automatically in  \code{\link{fnc_get_soil}}.
#' @param PTF_to_use which PTF will later be used in \code{\link{fnc_get_soil}} has an impact on the setting of oc.pct, so this information is passed down from \code{\link{fnc_get_soil}} here.
#' @param dgm df.dgm gets created in \code{\link{fnc_get_soil}} and is passed here to avoid complications with potential df.dgms in the global environment.
#'
#' @return Returns a list of soil data frames.

fnc_soil_stok <- function(df,
                          df.LEIT,
                          PTF_to_use,
                          dgm){
  ls.soils.tmp <- list()
  for (i in 1:nrow(df)){
    df.soil <- tryCatch({
      df.tmp <- df.LEIT %>%
        filter(RST_F == df$RST_F[i]) %>%
        dplyr::mutate("ID" = df$ID[i],
                      "ID_custom" = as.character(df$ID_custom[i])) %>%
        dplyr::mutate(TRD = round(as.numeric(TRD), 3)) %>%

        dplyr::select(ID, ID_custom, LAGENUM, TIEFE_OG, TIEFE_UG, SAND, SCHLUFF, TON, SKELETT, TRD, SOC, humusform) %>%
        setNames(c("ID", "ID_custom", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct", "humusform")) %>%

        dplyr::mutate_at(vars(-all_of(c("ID_custom", "humusform"))), as.numeric)

      # Tiefendiskretisierung, Slope & Aspect
      df.tmp <- fnc_depth_disc(df.tmp) %>%
        dplyr::mutate(oc.pct = case_when((is.na(oc.pct)|oc.pct==-9999) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                         (is.na(oc.pct)|oc.pct==-9999) & PTF_to_use == "HYPRES" ~ 0.1,
                                         T ~ oc.pct),
                      humus = case_when(humusform == "Mull" ~ 0.03,
                                        humusform == "Mullmoder" ~ 0.067,
                                        humusform == "Moder" ~ 0.045,
                                        humusform == "Rohhumusartiger Moder" ~ 0.06,
                                        humusform == "Rohhumus" ~ 0.07,
                                        T ~ 0),
                      upper = upper/-100,
                      lower = lower/-100,
                      gravel = gravel / 100) %>%
        dplyr::left_join(dgm, by = "ID") %>%
        dplyr::select(ID, ID_custom, mat, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus) %>%
        as_tibble()

    },
    error = function(cond){
      message(cat("\n RST_F ", df$RST_F[i], " seems to cause issues. row ", i))
      message(cond)
      return(NULL)
    })

    ls.soils.tmp[[i]] <- df.soil

  }
  return(ls.soils.tmp)
}
