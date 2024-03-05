#' Function to create list of climate arguments for polygons as input
#'
#' When climate data, as needed in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, isn't provided as a list (which takes up ram if too many points are modelled), but as a list of climate arguments to be read out by \code{\link{fnc_read_climdb_poly}}, this function creates the necessary argument list. Polygons usually need the input of several Climate-Cells. Which ones and how much weight each cell contributes to the climate within the polygons is determined here.
#'
#'
#' @param df.ids data frame with \code{ID_custom}, \code{id_standard}, \code{tranche} (if \code{df.ids} is created by \code{\link{fnc_create_IDs}}, these columns are already there. Must otherwise be created with \code{\link{fnc_relateCoords}}), and \code{lower}
#' @param mindate used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$startdate}.
#' @param maxdate used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$enddate}.
#' @param store_as when the file should be stored somewhere, a complete path with file ending \code{".rds"} must be provided, else if \code{NA}, list will be returned to the console
#'
#' @return list of climate arguments as needed by \code{\link{fnc_read_climdb_poly}}
#' @export


fnc_get_climate_args_poly <- function(df.ids,
                                      mindate,
                                      maxdate,
                                      store_as = NA){
  # sort dfs according to IDs
  df.ids$ID <- 1:nrow(df.ids)

  UHH_empty <- terra::rast(paste0(path_UHH, "UHH_ras.tif"))
  standard_locations <- readRDS(paste0(path_UHH, "std_locations.rds"))

  ls.assign <- rbindlist(
    mapply(
      exactextractr::exact_extract(UHH_empty,
                                   df.ids,
                                   include_xy =T),

      FUN = function(x, id, id_name){
        x$ID = id
        x$ID_custom = id_name
        x$id_standard = paste0(x$x, x$y)
        return(x[,c( "ID","ID_custom","id_standard", "coverage_fraction")])
      },

      id = df.ids$ID,
      id_name = df.ids$ID_custom,
      SIMPLIFY = F))

  # join with available data
  ls.assign <- dplyr::left_join(ls.assign, standard_locations, by=  "id_standard") %>%
    group_split(ID)

  # when only cells are touched, that are not available, because of non-forest-assignment
  nearest <- which(unlist(lapply(ls.assign, function(x){ifelse(all(is.na(x$tranche)), 1,0)})) == 1)

  if(length(nearest) > 0){
    sf_cent <- st_centroid(df.ids[nearest,"ID"])%>%
      mutate(X = round(st_coordinates(.)[, "X"]),
             Y = round(st_coordinates(.)[, "Y"]))

    cents_on_polys <- st_intersection(sf_cent, df.ids) %>%
      filter(ID == ID.1) %>% pull(ID)

    sf_points <- st_point_on_surface(df.ids[nearest, "ID"]) %>%
      filter(!ID %in% cents_on_polys)
    if(nrow(sf_points) > 0){
      sf_points <- sf_points %>%
        mutate(X = round(st_coordinates(.)[, "X"]),
               Y = round(st_coordinates(.)[, "Y"]))
    }

    sf_cent <- sf_cent %>%
      filter(ID %in% cents_on_polys) %>%
      bind_rows(., sf_points) %>%
      arrange(ID) %>%
      st_transform(32632)

    standard_locations_sf <- sf::st_as_sf(standard_locations,
                                          crs = 32632,
                                          coords = c("x_standard",
                                                     "y_standard"))



    # get index of std-locations nearest to cst-locations ----
    standard_locations_ix <- sf::st_nearest_feature(x = sf_cent,
                                                    y = standard_locations_sf)
    standard_locations_sample <- standard_locations[standard_locations_ix,]

    # concatenate relations ----
    location_relations <- cbind(df.ids$ID[nearest],
                                df.ids$ID_custom[nearest],
                                rep(1, length(nearest)),
                                standard_locations_sample)
    colnames(location_relations) <- c("ID",
                                      "ID_custom",
                                      "coverage_fraction",
                                      "id_standard",
                                      "x_standard",
                                      "y_standard",
                                      "tranche")
    location_relations <- location_relations %>%
      dplyr::select(ID, ID_custom, id_standard, coverage_fraction, x_standard, y_standard, tranche) %>%
      group_split(ID)
  }

  # remove non-available cells
  ls.assign <- lapply(ls.assign, function(x){na.omit(x)})

  # add closest-cell-data
  if(length(nearest) > 0){
    for(i in 1:length(nearest)){
      ls.assign[[nearest[i]]] <- as.list(location_relations[i])[[1]]
    }
  }

  # append and sort
  names(ls.assign) <- as.character(df.ids$ID_custom)

  # remove fractions of cells up to 5% of the whole polygon area
  # (reduces the number of dfs to read and summarise, but keeps their significance minimal)

  ls.assign <- lapply(ls.assign, FUN = function(x){

        x$w = x$coverage_fraction/sum(x$coverage_fraction)
        x <- x[rev(order(x$w)),]
        x$cumsum <- cumsum(x$w)

        return(x[1:which(x$cumsum > 0.95)[1], ])
      })

  # format
  clim_args <-
    lapply(ls.assign,
           function(x) list(IDs = x$ID_custom[1],
                            clim_dir = paste0(path_clim, "tr", x$tranche, "/", x$id_standard, ".rds"),
                            weights = x$w,
                            mindate = mindate,
                            maxdate = maxdate))

  if(!is.na(store_as)){
    saveRDS(object = clim_args, file = store_as)
  }else{
    return(clim_args)
  }
}
