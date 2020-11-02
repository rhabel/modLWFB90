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
    df.soil <- df.LEIT %>%
      filter(RST_F == df$RST_F[i]) %>%
      dplyr::mutate("ID" = df$ID[i],
                    "ID_custom" = as.character(df$ID_custom[i])) %>%

      dplyr::select(ID, ID_custom, LAGENUM, TIEFE_OG, TIEFE_UG, SAND, SCHLUFF, TON, SKELETT, TRD, SOC) %>%
      setNames(c("ID", "ID_custom", "mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct")) %>%

      dplyr::mutate_at(vars(-matches("ID_custom")), as.numeric)

    # Tiefendiskretisierung, Slope & Aspect
    df.soil <- fnc_depth_disc(df.soil) %>%
      dplyr::mutate(oc.pct = case_when(is.na(oc.pct) & PTF_to_use == "PTFPUH2" ~ 0.5,
                                is.na(oc.pct) & PTF_to_use == "HYPRES" ~ 0.1,
                                T ~ oc.pct)) %>%
      dplyr::left_join(dgm, by = "ID")

    # Humus: ...
    df.soil <- df.soil %>%
      dplyr::mutate(humus = 0.05,
                    upper = upper/-100,
                    lower = lower/-100,
                    gravel = gravel / 100) %>%
      dplyr::select(ID, ID_custom, mat, upper, lower, sand, silt, clay, gravel, bd, oc.pct, aspect, slope, humus)


    ls.soils.tmp[[i]] <- as_tibble(df.soil)
  }
  return(ls.soils.tmp)
}
