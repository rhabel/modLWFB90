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
#' } \cr OR: A simple feature with any of the columns above. If df.ids is a simple feature, site information "aspect" and "slope" will be averaged over the polygon area. Coord_y and coord_x will be taken from the centroid.
#'
#' @param tree_species name of the tree species to be modelled with. Either a single species name that is then used for all points in \code{df.ids}, or a vector of the same length as \code{df.ids$ID} if the main tree species of each point is known and differs. Must be one of \code{beech}, \code{oak}, \code{spruce}, \code{pine}, \code{larch}, \code{douglasfir}. \cr It is  recommended (findings of project WHH-KW) to use the settings of spruce for fir and the settings of pine for larch. If certain tree specific parameters should be changed permanently, the corresponding object \code{params_...} should be overwritten manually before running the function.
#' @param df.ind.info a data frame containing individual site information for each ID or if parameters are presumed to be different than the default settings of \code{\link[LWFBrook90R]{set_paramLWFB90}}. They need to be given here in the form of a data frame containing the column \code{ID}, which should be identical to the \code{ID}-column of\code{df.ids}, and additional columns that are named exactly like the parameters in \code{\link[LWFBrook90R]{set_paramLWFB90}}.
#' @param wuchsmaechtigkeit function in progress. provides an approximation for the mean growth of an area in BW. Adjusts LAI, SAI and max height of the vegetation. Default is \code{F}, as still in progress.
#'
#' @return Returns a list of parameter settings that can be read and further processed by \code{\link[LWFBrook90R]{run_multisite_LWFB90}} or \code{\link[LWFBrook90R]{run_LWFB90}}
#' @example inst/examples/fnc_get_params_ex.R
#'
#' @export
# df.ids <- st_read("J:/FVA-Projekte/P01765_StWM_KPW/Daten/Urdaten/GIS/Exkursionsgebiet/Stoka.shp")[1:3,] %>%
#   mutate(ID_custom = paste0("ID_0", 1:nrow(.))) %>% select(ID_custom)



fnc_get_params <- function(df.ids,

                           tree_species = "spruce",
                           df.ind.info = NULL){

  # IDs okay? ------------------------------------------------- ####
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  # df or sf? ------------------------------------------------- ####
  if(all(c("sf", "data.frame") %in% class(df.ids))){
    is_sf = T
  }else{
    is_sf = F
  }

  if(!is.null(df.ind.info)){
    if(!identical(df.ids$ID_custom, df.ind.info$ID_custom)){
      stop("IDs of df.ids and df.ind.info are not identical.")
    }
  }

  if(is_sf){
    sf_cent <- st_centroid(df.ids[,"ID"])%>%
      mutate(X = round(st_coordinates(.)[, "X"]),
             Y = round(st_coordinates(.)[, "Y"]))

    cents_on_polys <- st_intersection(sf_cent, df.ids) %>%
      filter(ID == ID.1) %>% pull(ID)

    sf_points <- st_point_on_surface(df.ids[,"ID"]) %>%
      filter(!ID %in% cents_on_polys)
    if(nrow(sf_points) > 0){
      sf_points <- sf_points %>%
        mutate(X = round(st_coordinates(.)[, "X"]),
               Y = round(st_coordinates(.)[, "Y"]))
    }

    sf_cent <- sf_cent %>%
      filter(ID %in% cents_on_polys) %>%
      bind_rows(., sf_points) %>% arrange(ID)

  }


  # SLOPE & ASPECT -------------------------------------------- ####
  if(is_sf){

    # df.ids as polygons -> exactextractr::exact_extract
    # transform to GK3-terra object
    df.dgm <- df.ids[,"ID"] %>% sf::st_transform(25832)
    #df.dgm <- terra::vect(df.dgm)

    # extract
    aspect <- terra::rast(paste0(path_DGM, "aspect.tif"))
    slope <- terra::rast(paste0(path_DGM, "slope.tif"))

    df.dgm$slope <- round(exactextractr::exact_extract(slope, df.dgm, 'mean'))
    options(warn = -1)
    df.dgm$aspect <- round(exactextractr::exact_extract(aspect, df.dgm, summarize_df = FALSE,
                                                  fun = custom_aspect_func))

    df.dgm <- df.dgm %>% st_drop_geometry()

    # append dgm
    df.ids <- dplyr::left_join(df.ids, df.dgm, by = "ID")


  }else{

    # df.ids as df -> simple extract
    if(!all(c("slope", "aspect") %in% colnames(df.ids))){
      # transform to GK3-terra object
      df.dgm <- sf::st_as_sf(df.ids,
                             coords = c("easting", "northing"), crs = 32632) %>%
        sf::st_transform(25832)
      df.dgm <- terra::vect(df.dgm)

      # extract
      dgm_spat <- terra::rast(list.files(path_DGM, pattern = "aspect.tif|slope.tif", full.names=T))
      df.dgm <- round(terra::extract(dgm_spat, df.dgm), 0)

      # append dgm
      df.ids <- dplyr::left_join(df.ids, df.dgm, by = "ID")

    }

  }


  # LAT & LON ------------------------------------------------- ####
  if(is_sf){

    df.latlon <- cbind("ID" = df.ids$ID,
                       setNames(as.data.frame(sf::st_coordinates(sf::st_transform(sf_cent, crs = 4326))),
                                c("coords_x", "coords_y")))
    df.ids <- dplyr::left_join(df.ids, df.latlon, by = "ID")

  }else if(!all(c("coords_x", "coords_y") %in% colnames(df.ids))){

    sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632)
    df.latlon <- cbind("ID" = df.ids$ID,
                       setNames(as.data.frame(sf::st_coordinates(sf::st_transform(sf.ids, crs = 4326))),
                                c("coords_x", "coords_y")))
    df.ids <- dplyr::left_join(df.ids, df.latlon, by = "ID")

  }

  # prepare tree type ----------------------------------------- ####
  if(is_sf){
    df.ids <- df.ids %>% st_drop_geometry()
  }


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



  # add pdur ------------------------------------------------- ####
  if(!"pdur" %in% colnames(df.ids)){

    # transform points to spatvect
    if(is_sf){
      sf.ids <- sf_cent %>%
        sf::st_transform(25832)
    }else{
      sf.ids <- sf::st_as_sf(df.ids, coords = c("easting", "northing"), crs = 32632) %>%
        sf::st_transform(25832)
    }

    spat.ids <- terra::vect(sf.ids)

    # load pdur-rasters.
    out <- terra::rast(paste0(path_pdur, "pdur.tif"))

    extr_vals <- terra::extract(out, spat.ids)
    extr_vals[is.na(extr_vals)] <- 4
    extr_vals <- split(extr_vals[,2:13], seq(nrow(extr_vals)))
    extr_vals <- lapply(extr_vals, function(x) as.numeric(x))

    rm(out); gc()

    ls.param <- mapply(FUN = function(liste, vals){
                         liste$pdur <- vals
                         return(liste)
                       },
                       liste = ls.param,
                       vals = extr_vals,
                       SIMPLIFY = F)
  }

  names(ls.param) <- df.ids$ID_custom

  return(ls.param)
}

# function for mean aspect ---------------------------------------------------- ####
custom_aspect_func <- function(values, cov_frac){
  out <- circular::weighted.mean.circular(rad(values), cov_frac, modulo = "2pi", na.rm = T) *180/pi
  if(out<0){
    out <-out+360
  }
  return(out)
}
