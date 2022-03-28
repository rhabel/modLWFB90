#' Create ID-raster pattern within polygon with set resolution
#'
#' This function creates a regular pattern of modelling points for a given test area and a given resolution.
#'
#' @param poly polygon of the test area in EPSG:32632 as full path with file name and ending. Accepts all files that can be read with \code{sf::st_read}
#' @param res resolution in m
#' @param out_dir directory where df.ids shall be stored as \code{.rds}, if set to \code{NA}, data frame(s) will be returned to the console
#' @param ID_pre optional, prefix for ID_custom-name as character. Default is \code{ID_}
#' @param out_name optional, name for ID-files, default is \code{IDs.rds}
#' @param add_tranches when large areas are modelled with a high resolution, it can be useful to model your area as tranches similar to a SUDOKU-field. This way you can calculate the results tranche by tranche and in a 9th of the total computing time you get a result covering the whole modelling area. In this case the IDs are assigned to a number of tranches set by \code{tranches}. Tranches must be square number, such as 4, 9, 16, or 25. Default is \code{NA} without tranches, otherwise several df.ids are stored as tranches named \code{out_name_trX.rds} in \code{out_dir}.
#'
#' @return returns a list of points with ID_custom (optional), x- and y- coordinates
#' @examples
#' df.ids <- fnc_create_IDs(poly = "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/GIS/metadaten/Freiburg_Wald.shp",
#'                          res = 100,
#'                          ID_pre = "ID_",
#'                          out_dir = NA,
#'                          out_name = "IDs",
#'                          tranches = 16)
#'
#' sf.ids <- sf::st_as_sf(df.ids,
#'                        coords = c("easting", "northing"),
#'                        crs = 32632)
#'
#' ggplot(sf.ids)+
#'    geom_sf(aes(color = factor(Tranche)))
#'
#' @export

fnc_create_IDs <- function(poly,
                           res = 50,
                           ID_pre = "ID_",
                           out_dir = NA,
                           out_name = "IDs",
                           tranches = NA){

  shptmp <- sf::st_read(poly)


  # boundaries
  bbox <- sf::st_bbox(shptmp)

  # Easting
  start <- trunc(bbox[1])
  stop <- ceiling(bbox[3])
  step <- res
  x <- start

  easting <- c(start)
  while (x < stop) {
    x <- max(easting) + step
    easting <- append(easting, x)
  }

  # Northing
  start <- trunc(bbox[2])
  stop <- ceiling(bbox[4])
  step <- res
  y <- start

  northing <- c(start)
  while (y < stop) {
    y <- max(northing) + step
    northing <- append(northing, y)
  }


  rm(list = c("bbox", "start", "stop", "x", "y")); gc()

  # all points --------------------------- ####
  df.ids <- data.frame(easting = rep(easting, each = length(northing)),
                       northing = rep(northing, length(easting)))

  df.ids$ID <- 1:nrow(df.ids)

  # reduce to which are in polygon ----- ####

  # create spatial point data frame containing all ids
  ids_complete_spatial <- terra::vect(df.ids,
                                      geom = c("easting", "northing"),
                                      crs = "EPSG:32632")

  # polygon file to spatial object
  polygon <- terra::vect(shptmp)

  # select ids inside polygon by intersection
  ids_in_polygon_spatial <- terra::intersect(x = ids_complete_spatial, y = polygon)

  # select ids
  ids_in_polygon <- ids_in_polygon_spatial$ID

  rm(list = c("ids_complete_spatial", "polygon", "ids_in_polygon_spatial", "shptmp")); gc()

  df.ids <- df.ids %>%
    dplyr::filter(ID %in% ids_in_polygon)

  # add complete IDs-column
  df.ids$ID <- 1:nrow(df.ids)
  df.ids$ID_custom <- paste0(ID_pre,
                             stringr::str_pad(1:nrow(df.ids),
                                              width = nchar(nrow(df.ids)),
                                              pad = "0"))
  # # # plot
  # sf.ids <- sf::st_as_sf(df.ids,
  #                        coords = c("easting", "northing"),
  #                        crs = 32632)
  # plot(sf.ids["ID"])


  if(is.na(tranches)){

    if (is.na(out_dir)){

      return(df.ids)

    }else{

      if(stringr::str_sub(out_dir, -1,-1) != "/"){
        out_dir <- paste0(out_dir, "/")
      }

      saveRDS(df.ids,
              file = paste0(out_dir, out_name, ".rds"))

    }
  }else{
    # to Tranche -------------------- ####

    for(i in 1:sqrt(tranches)){
      assign(paste0("x", i),  easting[seq(i, length(easting), sqrt(tranches))])
      assign(paste0("y", i),  northing[seq(i, length(northing), sqrt(tranches))])
    }


    count_x = rep(1:sqrt(tranches), each = sqrt(tranches))
    count_y = rep(1:sqrt(tranches), sqrt(tranches))
    ls_emp <- list()

    for(i in 1:tranches){

      ls_emp[[i]] <- data.frame(easting = rep(get(paste0("x",count_x[i])),
                                              each = length(get(paste0("y", count_y[i])))),
                                northing = rep(get(paste0("y", count_y[i])),
                                               length(get(paste0("x", count_x[i])))),
                                Tranche = rep(as.numeric(i),
                                              length(rep(get(paste0("x",count_x[i])),
                                                         each = length(get(paste0("y", count_y[i])))))))
    }

    ids2 <- do.call(rbind, ls_emp)

    # merge with df.ids
    df.ids <- dplyr::left_join(df.ids, ids2,
                               by = c("easting", "northing"))

    # # # # plot
    # sf.ids <- sf::st_as_sf(df.ids,
    #                        coords = c("easting", "northing"),
    #                        crs = 32632)
    # plot(sf.ids["Tranche"])

    ####----- subset


    if (is.na(out_dir)){

      # ls.ids <- df.ids %>%
      #   dplyr::group_split(Tranche)

      return(df.ids)

    }else{

      if(stringr::str_sub(out_dir, -1,-1) != "/"){
        out_dir <- paste0(out_dir, "/")
      }


      for(i in 1:tranches){
        ids_tmp <- df.ids[df.ids$Tranche == i, ]
        saveRDS(ids_tmp, file = paste0(out_dir, out_name, "_tr", i, ".rds"))
      }

    }

  }

}
