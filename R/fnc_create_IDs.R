#' Create ID-raster pattern within polygon with set resolution
#'
#' This function creates a regular pattern of modelling points for a given test area and a given resolution.
#'
#' @param poly polygon of the test area in EPSG:32632 as full path with file name and ending. Accepts all files that can be read with \code{sf::st_read}
#' @param res resolution in m
#' @param out_dir directory where df.ids shall be stored as \code{.rds}, if set to \code{NA}, a data frame will be returned to the console
#' @param ID_pre optional, prefix for ID_custom-name as character. Default is \code{ID_}
#' @param out_name optional, name for ID-files, default is \code{IDs.rds}
#' @param add_tranches when large areas are modelled with a high resolution, it can be useful to model your area as tranches. In this case the IDs are assigned to 9 Tranches, similar to a SUDOKU-field. This way you can calculate the results tranche by tranche and in a 9th of the total computing time you get a result covering the whole modelling area. If \code{TRUE}, df.ids are stored as 9 tranches named \code{out_name _ trX.rds} in \code{out_dir}.
#'
#' @return returns a list of points with ID_custom (optional), x- and y- coordinates
#'
#'
#' @export

fnc_create_IDs <- function(poly,
                           res = 250,
                           ID_pre = "ID_",
                           out_dir = NA,
                           out_name = "IDs",
                           add_tranches = F){

  shptmp <- sf::st_read(poly)

  # boundaries
  bbox <- st_bbox(shptmp)

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

  # all points --------------------------- ####
  df.ids <- data.frame(easting = rep(easting, each = length(northing)),
                    northing = rep(northing, length(easting)))

  df.ids$ID_custom <- paste0(ID_pre,
                             str_pad(1:nrow(df.ids),
                                     width = nchar(nrow(df.ids)),
                                     pad = "0"))

  # reduce to which are in polygon ----- ####

  # create spatial point data frame containing all ids
  ids_complete_spatial <- sf::st_as_sf(df.ids,
                                       coords = c("easting", "northing"),
                                       crs = 32632) %>%
    sf::as_Spatial(IDs = df.ids$ID_custom)

  # polygon file to spatial object
  polygon <- sf::as_Spatial(shptmp)

  # select ids inside polygon by intersection
  ids_in_polygon_spatial <- raster::intersect(x = ids_complete_spatial, y = polygon)

  # select ids
  ids_in_polygon <- ids_in_polygon_spatial$ID_custom

  df.ids <- df.ids %>%
    dplyr::filter(ID_custom %in% ids_in_polygon)

  # add complete IDs-column
  df.ids$ID <- 1:nrow(df.ids)
  df.ids$ID_custom <- paste0(ID_pre,
                             str_pad(1:nrow(df.ids),
                                     width = nchar(nrow(df.ids)),
                                     pad = "0"))
  # # # plot
  # sf.ids <- sf::st_as_sf(df.ids,
  #                        coords = c("easting", "northing"),
  #                        crs = 32632)
  # plot(sf.ids["ID"])

  # return df.ids:

  if (is.na(out_dir)){
    return(df.ids)
  }else{
    # prepare writing
    if(stringr::str_sub(out_dir, -1,-1) != "/"){
      out_dir <- paste0(out_dir, "/")
    }



    if(add_tranches == F){
      saveRDS(df.ids,
              file = paste0(out_dir, out_name, ".rds"))
    }else{
      # to Tranche -------------------- ####
      # replicate Easting
      x1 <- easting[seq(1, length(easting), 3)]
      x2 <- easting[seq(2, length(easting), 3)]
      x3 <- easting[seq(3, length(easting), 3)]

      # replicate Northing
      y1 <- northing[seq(1, length(northing), 3)]
      y2 <- northing[seq(2, length(northing), 3)]
      y3 <- northing[seq(3, length(northing), 3)]

      # merge
      tr1 <- data.frame(easting = rep(x1, each = length(y1)),
                        northing = rep(y1, length(x1)),
                        Tranche = rep("tr1", length(rep(x1, each = length(y1)))))

      tr2 <- data.frame(easting = rep(x1, each = length(y2)),
                        northing = rep(y2, length(x1)),
                        Tranche = rep("tr2", length(rep(x1, each = length(y2)))))

      tr3 <- data.frame(easting = rep(x1, each = length(y3)),
                        northing = rep(y3, length(x1)),
                        Tranche = rep("tr3", length(rep(x1, each = length(y3)))))

      tr4 <- data.frame(easting = rep(x2, each = length(y1)),
                        northing = rep(y1, length(x2)),
                        Tranche = rep("tr4", length(rep(x2, each = length(y1)))))

      tr5 <- data.frame(easting = rep(x2, each = length(y2)),
                        northing = rep(y2, length(x2)),
                        Tranche = rep("tr5", length(rep(x2, each = length(y2)))))

      tr6 <- data.frame(easting = rep(x2, each = length(y3)),
                        northing = rep(y3, length(x2)),
                        Tranche = rep("tr6", length(rep(x2, each = length(y3)))))

      tr7 <- data.frame(easting = rep(x3, each = length(y1)),
                        northing = rep(y1, length(x3)),
                        Tranche = rep("tr7", length(rep(x3, each = length(y1)))))

      tr8 <- data.frame(easting = rep(x3, each = length(y2)),
                        northing = rep(y2, length(x3)),
                        Tranche = rep("tr8", length(rep(x3, each = length(y2)))))

      tr9 <- data.frame(easting = rep(x3, each = length(y3)),
                        northing = rep(y3, length(x3)),
                        Tranche = rep("tr9", length(rep(x3, each = length(y3)))))


      ids2 <- rbind(tr1, tr2, tr3, tr4, tr5, tr6, tr7, tr8, tr9)


      # merge with df.ids
      df.ids <- dplyr::left_join(df.ids, ids2,
                                 by = c("easting", "northing"))

      # # # plot
      # sf.ids <- sf::st_as_sf(df.ids,
      #                        coords = c("easting", "northing"),
      #                        crs = 32632)
      # plot(sf.ids["Tranche"])

      ####----- subset

      ids_tr1 <- df.ids[df.ids$Tranche == "tr1",]
      ids_tr2 <- df.ids[df.ids$Tranche == "tr2",]
      ids_tr3 <- df.ids[df.ids$Tranche == "tr3",]
      ids_tr4 <- df.ids[df.ids$Tranche == "tr4",]
      ids_tr5 <- df.ids[df.ids$Tranche == "tr5",]
      ids_tr6 <- df.ids[df.ids$Tranche == "tr6",]
      ids_tr7 <- df.ids[df.ids$Tranche == "tr7",]
      ids_tr8 <- df.ids[df.ids$Tranche == "tr8",]
      ids_tr9 <- df.ids[df.ids$Tranche == "tr9",]


      ####----- save
      saveRDS(ids_tr1, file = paste0(out_dir, out_name, "_tr1.rds"))
      saveRDS(ids_tr2, file = paste0(out_dir, out_name, "_tr2.rds"))
      saveRDS(ids_tr3, file = paste0(out_dir, out_name, "_tr3.rds"))
      saveRDS(ids_tr4, file = paste0(out_dir, out_name, "_tr4.rds"))
      saveRDS(ids_tr5, file = paste0(out_dir, out_name, "_tr5.rds"))
      saveRDS(ids_tr6, file = paste0(out_dir, out_name, "_tr6.rds"))
      saveRDS(ids_tr7, file = paste0(out_dir, out_name, "_tr7.rds"))
      saveRDS(ids_tr8, file = paste0(out_dir, out_name, "_tr8.rds"))
      saveRDS(ids_tr9, file = paste0(out_dir, out_name, "_tr9.rds"))
    }

  }


}
