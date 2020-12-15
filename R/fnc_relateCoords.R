#' @title Relate coordinates to locations in climatic database
#' @description Relate custom locations of interest to standard locations present in a climatic database by nearest neighbour operation.
#' @description The standard locations represent the center points of underlying climatic raster data cells located in forest areas of Baden-Württemberg.
#'
#' @param df.ids a data frame containing the following columns:
#' \itemize{
#' \item \code{ID} - a numbered ID that is created in fnc_get_clim for unique assignment within functions
#' \item \code{ID_custom} - a unique ID-column for assignment that all intermediate products as well as the output will be assigned to.
#' \item \code{easting} and \code{northing} - coordinates in UTM EPSG:32632
#' }
#' @param path_std path to standard locations directory
#' @details Spatial resolution of the underlying raster data and therefore the grid of standard locations is 250 m. Coordinates reference system of the raster data is WGS84-UTM32N, EPSG:32632.
#' @details The climatic database provides daily information for several climatic variables at standard locations as input for the Brook90-model. The population of almost 230.000 standard locations is spatialised to 9 tranches, each tranche including approximately 25.000 locations.
#' @details Raster representation in the relation of custom to standard location is ensured \code{Value = 1}, as long as the custom location is inside the raster cell underlying the standard location. Therefore the distance from custom location to nearest standard location has to be <= sqrt((250/2)^2*2)) [m].
#' @references Dietrich, H., Wolf, T., Kawohl, T., Wehberg, J., Kändler, G., Mette, T., Röder, A., Böhner, J. (2019): “Temporal and spatial high-resolution climate data from 1961 to 2100 for the German National Forest Inventory (NFI)”, Annals of Forest Sience, 76:6. https://doi.org/10.1007/s13595-018-0788-5
#' @author Thilo Wolf <thilo.wolf@@forst.bwl.de>, adjusted by Raphael Habel <raphael.habel@@forst.bwl.de>
#' @return A data frame containing:
#' @return - id and coordinates of custom location and nearest standard location in CRS=EPSG:32632.
#' @return - membership of location in tranche.
#' @return - distance of custom location to the nearest standard location.
#' @return - raster representation in relation of locations.
#' @import sf
#' @import tidyverse
#' @examples
#'
#' fnc_relateCoords(df.ids = test.ids.bds)
#' @export



fnc_relateCoords <- function(df.ids,
                             path_std = "R:/klima/whh/brook90_input/locations") {

    # create df to store standard locations ----
    standard_locations <- data.frame(id_standard = character(),
                                     tranche = numeric(),
                                     x_standard = numeric(),
                                     y_standard = numeric(),
                                     stringsAsFactors = FALSE)

    # loop tranches ----
    for (tranche in 1:9) {

        # testing
        # tranche <- 1

        # read coordinates of standard locations ----
        xy_standard <- read.csv(file = paste0(path_std,
                                              "/x_y_tr",
                                              tranche,
                                              ".txt"),
                                header = FALSE,
                                sep = "\t",
                                col.names = c("x_standard",
                                              "y_standard"))

        # create coordinates based id ----
        id_standard <- paste0(xy_standard$x_standard,
                              xy_standard$y_standard)

        # concatenate id, tranche, coordinates ----
        standard_locations_loop <- cbind(id_standard,
                                         xy_standard,
                                         tranche)

        # append current df (current tr) to final df (all tr) ----
        standard_locations <- rbind.data.frame(standard_locations,
                                               standard_locations_loop)
    }

    # standard locations to sf-object ----
    standard_locations_sf <- sf::st_as_sf(standard_locations,
                                          crs = 32632,
                                          coords = c("x_standard",
                                                     "y_standard"))

    # custom locations to sf-object ----
    custom_locations_sf <- sf::st_as_sf(df.ids,
                                        coords = c("easting", "northing"),
                                        crs = 32632)


    # get index of std-locations nearest to cst-locations ----
    standard_locations_ix <- sf::st_nearest_feature(x = custom_locations_sf,
                                                    y = standard_locations_sf)

    # get sample of std-locations nearest to cst-locations ----
    standard_locations_sf_sample <- standard_locations_sf[standard_locations_ix,]
    standard_locations_sample <- standard_locations[standard_locations_ix,]

    # get distance from cst- to std-locations ----
    distance <- sf::st_distance(custom_locations_sf,
                                standard_locations_sf_sample,
                                by_element = TRUE)

    # set distance threshold for raster representation ----
    distance_threshold <- sqrt((250/2)^2*2)

    # concatenate relations ----
    location_relations <- cbind(standard_locations_sample,
                                df.ids$ID,
                                df.ids$ID_custom,
                                sf::st_coordinates(custom_locations_sf),
                                distance,
                                distance_threshold)

    # distance threshold as units-object ----
    location_relations$distance_threshold <- units::as_units(
        location_relations$distance_threshold,
        "m")

    # set raster representation ----
    location_relations$raster_representation <- ifelse(location_relations$distance <= location_relations$distance_threshold, 1, 0)

    # rename columns ----
    colnames(location_relations) <- c("id_standard",
                                      "x_standard",
                                      "y_standard",
                                      "tranche",
                                      "ID",
                                      "ID_custom",
                                      "x_custom",
                                      "y_custom",
                                      "distance",
                                      "distance_threshold",
                                      "raster_representation"
                                      )

    location_relations <- location_relations %>%
      dplyr::select(ID, ID_custom, x_custom, y_custom, tranche, id_standard, x_standard, y_standard, distance, distance_threshold, raster_representation) %>%
      dplyr::mutate(id_standard = as.character(id_standard))


    # return resulting df ----
    return(location_relations)
}


