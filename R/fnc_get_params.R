#' @title Parameter-list creation
#'
#' @description This function creates a list of parameter settings. Vegetation settings, by default, represent the findings of the WHH-KW project.
#'
#' @param df.ids a data frame that contains at least the following columns:
#' \itemize{
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' } \cr and may further contain the following meta information:
#' \itemize{
#' \item \code{slope} - slope at the modelling points in degree
#' \item \code{aspect} - aspect of the modelling points in degree
#' \item \code{coord_x} - longitude of points in degree, will be calculated automatically if missing
#' \item \code{coord_y} - latitude of points in degree, will be calculated automatically if missing
#' }
#'
#' @param tree_species name of the tree species to be modelled with. Either a single species name that is then used for all points in \code{df.ids}, or a vector of the same length as \code{df.ids$ID} if the main tree species of each point is known and differs. Must be one of \code{beech}, \code{oak}, \code{spruce}, \code{pine}, \code{larch}, \code{douglasfir}. \cr It is  recommended (findings of project WHH-KW) to use the settings of spruce for fir and the settings of pine for larch. If certain tree specific parameters should be changed permanently, the corresponding object \code{params_...} should be overwritten manually before running the function.
#' @param df.ind.info a data frame containing individual site information for each ID or if parameters are presumed to be different than the default settings of \code{\link[LWFBrook90R]{set_paramLWFB90}}. They need to be given here in the form of a data frame containing the column \code{ID}, which should be identical to the \code{ID}-column of\code{df.ids}, and additional columns that are named exactly like the parameters in \code{\link[LWFBrook90R]{set_paramLWFB90}}.
#' @param wuchsmaechtigkeit function in progress. provides an approximation for the mean growth of an area in BW. Adjusts LAI, SAI and max height of the vegetation. Default is \code{F}, as still in progress.
#'
#' @return Returns a list of parameter settings that can be read and further processed by \code{\link[LWFBrook90R]{run_multisite_LWFB90}} or \code{\link[LWFBrook90R]{run_LWFB90}}
#' @example inst/examples/fnc_get_params_ex.R
#' @export

fnc_get_params <- function(df.ids,
                          tree_species = "spruce",
                          df.ind.info = NULL,
                          wuchsmaechtigkeit = F){

  # IDs okay? ------------------------------------------------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  if(!is.null(df.ind.info)){
    if(!identical(df.ids$ID_custom, df.ind.info$ID_custom)){
      stop("IDs of df.ids and df.ind.info are not identical.")
    }
  }



  # SLOPE & ASPECT -------------------------------------------- ####
  if(!all(c("slope", "aspect") %in% colnames(df.ids))){
    # transform to GK3-terra object
    df.dgm <- sf::st_as_sf(df.ids,
                           coords = c("easting", "northing"), crs = 32632) %>%
      sf::st_transform(31467)
    df.dgm <- terra::vect(df.dgm)

    # extract
    dgm_spat <- terra::rast(list.files(path_DGM, pattern = "aspect.sdat|slope.sdat", full.names=T))
    df.dgm <- round(terra::extract(dgm_spat, df.dgm), 0)

    # append dgm
    df.ids <- dplyr::left_join(df.ids, df.dgm, by = "ID")

  }

  # LAT & LON ------------------------------------------------- ####
  if(!all(c("coords_x", "coords_y") %in% colnames(df.ids))){
    sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632)
    df.latlon <- cbind("ID" = df.ids$ID,
                       setNames(as.data.frame(sf::st_coordinates(sf::st_transform(sf.ids, crs = 4326))),
                                c("coords_x", "coords_y")))
    df.ids <- dplyr::left_join(df.ids, df.latlon, by = "ID")

  }

  # prepare tree type ----------------------------------------- ####

  if(!wuchsmaechtigkeit){

    df.site.infos <- df.ids %>%
      dplyr::rename(eslope = slope) %>%
      dplyr::mutate(dslope = eslope,
                    tree_species = tree_species,
                    budburst_species = case_when(tree_species == "beech" ~ "Fagus sylvatica",
                                                 tree_species == "spruce" ~ "Picea abies (spaet)",
                                                 tree_species == "oak" ~ "Quercus robur",
                                                 tree_species == "pine" ~ "Pinus sylvestris",
                                                 tree_species == "larch" ~ "Larix decidua",
                                                 tree_species == "douglasfir" ~ "Picea abies (spaet)",
                                                 T ~ NA_character_))

    # add additional data if present ---------------------------- ####
    if(!is.null(df.ind.info)){
      df.site.infos <- df.site.infos %>%
        dplyr::left_join(df.ind.info, by = "ID_custom")
    }

    # ls.param list creation with respective tree parameters
    ls.param <- lapply(df.ids$ID, function(x, df.site.infos) {
      parms <- get(paste0("params_",  df.site.infos[df.site.infos$ID == x, "tree_species"]))
      parms[match(names(df.site.infos),names(parms), nomatch = 0)] <-
        df.site.infos[df.site.infos$ID == x, which(names(df.site.infos) %in% names(parms))]
      return(parms)
    }, df.site.infos = df.site.infos)

  }else{

    # join with bonitaetskarte ---------------------------------- ####
    # create sf
    sf.ids <- sf::st_as_sf(df.ids,
                           coords = c("easting", "northing"), crs = 32632)
    # load bonitaetskarte
    sf.boni <- sf::st_read(paste0(path_WGB_diss_shp, "wugebs.shp"), quiet = T)

    # spatial join
    sf.ids <-  sf.ids %>%
      sf::st_join(sf.boni) %>%
      sf::st_drop_geometry()# %>%
      # mutate(WugebNr = case_when(WugebNr == 5~1,
      #                            WugebNr == 7~2, T~3)) %>%
      # rename(boni = WugebNr)

    # join with df.ids
    df.ids <- df.ids %>%
      dplyr::left_join(sf.ids)

    #
    df.site.infos <- df.ids %>%
      dplyr::rename(eslope = slope) %>%
      dplyr::mutate(dslope = eslope,
                    tree_species = tree_species,
                    budburst_species = case_when(tree_species == "beech" ~ "Fagus sylvatica",
                                                 tree_species == "spruce" ~ "Picea abies (spaet)",
                                                 tree_species == "oak" ~ "Quercus robur",
                                                 tree_species == "pine" ~ "Pinus sylvestris",
                                                 tree_species == "larch" ~ "Larix decidua",
                                                 tree_species == "douglasfir" ~ "Picea abies (spaet)",
                                                 T ~ NA_character_))

    # add additional data if present ---------------------------- ####
    if(!is.null(df.ind.info)){
      df.site.infos <- df.site.infos %>%
        dplyr::left_join(df.ind.info, by = "ID_custom")
    }

    df.site.infos <- dplyr::left_join(df.site.infos, df.boni,
                                      by = c("tree_species", "boni"))

    # ls.param list creation with respective tree parameters
    ls.param <- lapply(df.ids$ID, function(x, df.site.infos) {
      parms <- get(paste0("params_",  df.site.infos[df.site.infos$ID == x, "tree_species"]))
      parms[match(names(df.site.infos),names(parms), nomatch = 0)] <-
        df.site.infos[df.site.infos$ID == x, which(names(df.site.infos) %in% names(parms))]
      return(parms)
    }, df.site.infos = df.site.infos)
  }


  names(ls.param) <- df.ids$ID_custom

  return(ls.param)
}
