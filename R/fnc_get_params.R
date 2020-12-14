#' @title Parameter-list creation
#'
#' @description This function creates a list of parameter settings. Vegetation settings, by default, represent the findings of the WHH-KW project.
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param tree_species name of the tree species to be modelled with. Either a single species name that is then used for all points in \code{df.ids}, or a vector of the same length as \code{df.ids$ID} if the main tree species of each point is known and differs. Must be one of \code{beech}, \code{oak}, \code{spruce}, \code{pine}, \code{larch}, \code{douglasfir}. \cr It is  recommended (findings of project WHH-KW) to use the settings of spruce for fir and the settings of pine for larch. If certain tree specific parameters should be changed permanently, the corresponding object \code{params_...} should be overwritten manually before running the function.
#' @param df.ind.info a data frame containing individual site information for each ID or if parameters are presumed to be different than the default settings of \code{\link[LWFBrook90R]{setparam_LWFB90}}. They need to be given here in the form of a data frame containing the column \code{ID}, which should be identical to the \code{ID}-column of\code{df.ids}, and additional columns that are named exactly like the parameters in \code{\link[LWFBrook90R]{setparam_LWFB90}}.
#'
#' @return Returns a list of parameter settings that can be read and further processed by \code{\link[LWFBrook90R]{msiterunLWFB90}} or \code{\link[LWFBrook90R]{runLWFB90}}
#' @example inst/examples/fnc_get_params_ex.R
#' @export

fnc_get_params <- function(df.ids,
                          tree_species,
                          df.ind.info = NULL){


  # IDs okay? ---------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  if(!is.null(df.ind.info)){
    if(!identical(df.ids$ID_custom, df.ind.info$ID_custom)){
      stop("IDs of df.ids and df.ind.info are not identical.")
    }
  }



  # SLOPE & ASPECT ------ ####
  xy_gk <- fnc_transf_crs(df = df.ids)

  dgm.stack <- raster::stack(list.files(input_paul, pattern = "aspect.sdat|slope.sdat", full.names=T))
  df.dgm <- cbind("ID" = df.ids$ID,
                  as.data.frame(fnc_extract_points_dgm(lay = dgm.stack, xy = xy_gk)))

  # LAT & LON ------------ ####
  sf.ids <- st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632)
  df.latlon <- cbind("ID" = df.ids$ID,
                     setNames(as.data.frame(sf::st_coordinates(sf::st_transform(sf.ids, crs = 4326))),
                              c("coords_x", "coords_y")))

  # join ---------------- ####
  df.site.infos <- dplyr::left_join(df.ids, df.dgm, by = "ID") %>%
    dplyr::rename(eslope = slope) %>%
    dplyr::mutate(dslope = eslope,
                  budburst.species = case_when(tree_species == "beech" ~ "Fagus sylvatica",
                                               tree_species == "spruce" ~ "Picea abies (spaet)",
                                               tree_species == "oak" ~ "Quercus robur",
                                               tree_species == "pine" ~ "Pinus sylvestris",
                                               tree_species == "larch" ~ "Larix decidua",
                                               tree_species == "douglasfir" ~ "Picea abies (spaet)",
                                               T ~ NA_character_),
                  tree_species = tree_species) %>%
    dplyr::left_join(df.latlon, by = "ID")

  # if additional data present
  if(!is.null(df.ind.info)){
    df.site.infos <- df.site.infos %>%
      dplyr::left_join(df.ind.info, by = "ID_custom")
    # solving double issue if aspect or slope are already privided:
    df.site.infos <- df.site.infos[!str_detect(colnames(df.site.infos), pattern = "\\.x")]
    colnames(df.site.infos)[which(str_detect(colnames(df.site.infos), pattern = "\\.y") == T)] <- stringr::str_sub(colnames(df.site.infos[str_detect(colnames(df.site.infos), pattern = "\\.y")]),1,-3)
  }

  # ls.param list creation ---- ####
  ls.param <- lapply(df.ids$ID, function(x, df.site.infos) {
    parms <- get(paste0("params_",  df.site.infos[df.site.infos$ID == x, "tree_species"]))
    parms[match(names(df.site.infos),names(parms), nomatch = 0)] <-
      df.site.infos[df.site.infos$ID == x, which(names(df.site.infos) %in% names(parms))]
    return(parms)
  }, df.site.infos = df.site.infos)
  names(ls.param) <- df.ids$ID_custom

  return(ls.param)
}
