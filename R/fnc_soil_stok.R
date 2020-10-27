#' Soil-list creation from STOKA data
#'
#' This function transforms data as currently stored in the LEITPROFIL-database and returns a list of soil data frames as recognised by \code{LWFBrook90R}. The list is further processed in \code{\link{fnc_create_soil}} by adding soil hydraulic information, humus, and fine roots and can then be read by \code{\link[LWFBrook90]{msiterun}}.
#'
#' @param df A data frame containing the columns \code{ID} as distinct assignment from the initial \code{df.ids} dataframe and the column \code{RST_F}, which is created in \code{\link{fnc_create_soil}} through a spatial join of the coordinates with the STOK-BW shapefile (or, at this stage, the shapefile of test areas).
#' @param df.LEIT a data frame containing LEITPROFILE. At this stage of development, the comprehensive BW-wide database is not complete yet, so the test area needs to be assigned here. However, this is done automatically in  \code{\link{fnc_create_soil}}.
#'
#' @return Returns a list of soil data frames.
#' @export

fnc_soil_stok <- function(df,
                          df.LEIT){
  ls.soils.tmp <- list()
  for (i in 1:nrow(df)){
    df.soil <- df.LEIT %>%
      filter(RST_F == df$RST_F[i]) %>%
      dplyr::mutate("ID" = df$ID[i]) %>%

      dplyr::select(ID, LAGENUM, TIEFE_OG, TIEFE_UG, SAND, SCHLUFF, TON, SKELETT, TRD, SOC) %>%
      setNames(c("ID","mat", "upper", "lower", "sand", "silt", "clay", "gravel", "bd", "oc.pct")) %>%

      dplyr::mutate_all(as.numeric)

    # Tiefendiskretisierung, Slope & Aspect
    df.soil <- fnc_depth_disc(df.soil) %>%
      dplyr::mutate(oc.pct = case_when(is.na(oc.pct) & PTF == "PTFPUH2" ~ 0.5,
                                is.na(oc.pct) & PTF == "HYPRES" ~ 0.1,
                                T ~ oc.pct)) %>%
      dplyr::left_join(df.dgm, by = "ID")

    # Humus: ...
    df.soil <- df.soil %>%
      dplyr::mutate(humus = 0.05,
                    upper = upper/-100,
                    lower = lower/-100)


    ls.soils.tmp[[i]] <- as_tibble(df.soil)
  }
  return(ls.soils.tmp)
}
