#' Create depth discretisation for soil data frame
#'
#' This function takes a soil data frame that contains the depth and thickness of the soil horizons and
#' creates a soil data frame that creates n modelling nodes with \code{thick_1} distance from the top to \code{disk_gr1},
#' n2 nodes with \code{thick_2} distance from the top to \code{disk_gr2}, and n3 nodes with \code{thick_3} distance below \code{disk_gr2},
#' while at the same time keeping the position of the soil horizons. Distances between nodes and soil horizon depths
#' that do not add up to another layer are added to the layer above.
#'
#' @param df data frame containing mandatory depth information on horizon number (\code{mat}), \code{upper} and \code{lower} as columns. Information of soil physics are optional and will be kept and returned.
#' @param thick_1 first set distance between nodes the function will build down to \code{disk_gr1}
#' @param thick_2 second set distance between nodes the function will build down to \code{disk_gr2}
#' @param thick_3 third set distance between nodes the function will build below \code{disk_gr2}
#' @param disk_gr1 border of the first thickness change form \code{thick_1} to \code{thick_2}
#' @param disk_gr2 border of the second thickness change form \code{thick_2} to \code{thick_3}
#' @return A soil - data frame with a set position of soil nodes.
#' @export
#'
#' @example inst/examples/fnc_depth_disc_ex.R

fnc_depth_disc <- function(df,
                        thick_1 = 5, thick_2 = 10, thick_3 = 20,
                        disk_gr1 = 50, disk_gr2 = 100){
  upper <- df$upper
  lower <- df$lower
  mat <- df$mat

  layers <- lower-upper
  thickness <- numeric(0)
  material <-  numeric(0)

  for (i in 1:length(layers)){
    if(lower[i] <= disk_gr1){
      if(layers[i] >= thick_1){
        # falls potentiell mehr als eine Schicht...
        nlayer <- (layers[i] %/% thick_1)-1 # nlayer ohne finlayer
        finlayer <- (layers[i] %% thick_1)+thick_1

        thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_1, nlayer),finlayer)
        material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
      }else{
        # sonst einfach eine Schicht mit schichtdicke layerss[i]
        thickness[length(thickness)+1] <- layers[i]
        material[length(material)+1] <- mat[i]
      }


    } else if(lower[i] <=disk_gr2){
      if(upper[i] >= disk_gr1){
        if(layers[i] >= thick_2){
          # falls potentiell mehr als eine Schicht...
          nlayer <- (layers[i] %/% thick_2)-1 # nlayer ohne finlayer
          finlayer <- (layers[i] %% thick_2)+thick_2

          thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_2, nlayer),finlayer)
          material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
        }else{
          # sonst einfach eine Schicht mit schichtdicke layerss[i]
          thickness[length(thickness)+1] <- layers[i]
          material[length(material)+1] <- mat[i]
        }


      } else if(upper[i] < disk_gr1){

        assigneddepth <- 0
        vardepth <- upper[i]
        while(vardepth < disk_gr1){
          thickness[length(thickness)+1] <- thick_1
          material[length(material)+1] <- mat[i]
          vardepth <- vardepth+thick_1
          assigneddepth <- assigneddepth+thick_1
        }
        if((layers[i]-assigneddepth) >=thick_2 ){
          # falls das, was nach den "unterdisk_gr1"-thick_1cm ueber ist mindestens eine weitere thick_2 cm schicht ergibt...
          # -> neue thick_2 cm schicht(en)
          nlayer <- ((layers[i]-assigneddepth) %/% thick_2)-1 # nlayer ohne finlayer
          finlayer <- ((layers[i]-assigneddepth) %% thick_2)+thick_2

          thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_2, nlayer),finlayer)
          material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
        }else if ((layers[i]-assigneddepth) >=thick_1 ){
          # falls das, was nach den "unterdisk_gr1"-thick_1cm ueber ist zu wenig material fuer thick_2 cm schicht ist,
          # aber genug fuer weitere thick_1 cm schicht ...
          # -> neue schicht mit dicke differenz
          thickness[length(thickness)+1] <- (layers[i]-assigneddepth)
          material[length(material)+1] <- mat[i]
        }else{
          # falls das, was nach den "unterdisk_gr1"-thick_1cm ueber ist zu wenig material fuer thick_2 cm schicht ist,
          # nicht genut fuer weitere thick_1 cm schicht ...
          # -> dicke noch auf voherige schicht obendrauf
          thickness[length(thickness)] <- thickness[length(thickness)]+(layers[i]-assigneddepth)
        }
      }
    } else {
      if(upper[i] >= disk_gr2){
        if(layers[i] >= thick_3){
          # falls potentiell mehr als eine Schicht...
          nlayer <- (layers[i] %/% thick_3)-1 # nlayer ohne finlayer
          finlayer <- (layers[i] %% thick_3)+thick_3

          thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_3, nlayer),finlayer)
          material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
        }else{
          # sonst einfach eine Schicht mit schichtdicke layerss[i]
          thickness[length(thickness)+1] <- layers[i]
          material[length(material)+1] <- mat[i]
        }


      } else if(upper[i] >= disk_gr1){
        assigneddepth <- 0
        vardepth <- upper[i]
        while(vardepth < disk_gr2){
          thickness[length(thickness)+1] <- thick_2
          material[length(material)+1] <- mat[i]
          vardepth <- vardepth+thick_2
          assigneddepth <- assigneddepth+thick_2
        }
        if((layers[i]-assigneddepth) >=thick_3 ){
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist mindestens eine weitere thick_3 cm schicht ergibt...
          # -> neue thick_2 cm schicht(en)
          nlayer <- ((layers[i]-assigneddepth) %/% thick_3)-1 # nlayer ohne finlayer
          finlayer <- ((layers[i]-assigneddepth) %% thick_3)+thick_3

          thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_3, nlayer),finlayer)
          material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
        }else if ((layers[i]-assigneddepth) >=thick_2 ){
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist zu wenig material fuer thick_3 cm schicht ist,
          # aber genug fuer weitere mindestens-thick_2 cm-schicht ...
          # -> neue schicht mit dicke differenz
          thickness[length(thickness)+1] <- (layers[i]-assigneddepth)
          material[length(material)+1] <- mat[i]
        }else{
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist zu wenig material fuer thick_3 cm schicht ist,
          # nicht genut fuer weitere thick_2 cm schicht ...
          # -> dicke noch auf voherige schicht obendrauf
          thickness[length(thickness)] <- thickness[length(thickness)]+(layers[i]-assigneddepth)
        }

      } else if(upper[i] < disk_gr1){
        assigneddepth <- 0
        vardepth <- upper[i]
        while(vardepth < disk_gr1){
          thickness[length(thickness)+1] <- thick_1
          material[length(material)+1] <- mat[i]
          vardepth <- vardepth+thick_1
          assigneddepth <- assigneddepth+thick_1
        }
        while(vardepth < disk_gr2){
          thickness[length(thickness)+1] <- thick_2
          material[length(material)+1] <- mat[i]
          vardepth <- vardepth+thick_2
          assigneddepth <- assigneddepth+thick_2
        }
        if((layers[i]-assigneddepth) >=thick_3 ){
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist mindestens eine weitere thick_3 cm schicht ergibt...
          # -> neue thick_2 cm schicht(en)
          nlayer <- ((layers[i]-assigneddepth) %/% thick_3)-1 # nlayer ohne finlayer
          finlayer <- ((layers[i]-assigneddepth) %% thick_3)+thick_3

          thickness[c((length(thickness)+1):(length(thickness)+nlayer+1))] <- c(rep(thick_3, nlayer),finlayer)
          material[c((length(material)+1):(length(material)+nlayer+1))] <- rep(mat[i], (nlayer+1))
        }else if ((layers[i]-assigneddepth) >=thick_2 ){
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist zu wenig material fuer thick_3 cm schicht ist,
          # aber genug fuer weitere mindestens-thick_2 cm-schicht ...
          # -> neue schicht mit dicke differenz
          thickness[length(thickness)+1] <- (layers[i]-assigneddepth)
          material[length(material)+1] <- mat[i]
        }else{
          # falls das, was nach den "unterdisk_gr2"-thick_2cm ueber ist zu wenig material fuer thick_3 cm schicht ist,
          # nicht genut fuer weitere thick_2 cm schicht ...
          # -> dicke noch auf voherige schicht obendrauf
          thickness[length(thickness)] <- thickness[length(thickness)]+(layers[i]-assigneddepth)
        }

      }
    }

  }
  df.soil <- data.frame("thickness" = thickness,
                        "mat" = material) %>%
    left_join(df, by = "mat")
  for(i in 1:(nrow(df.soil)-1)){
    df.soil[i+1, "upper"] <- round(df.soil[i, "upper"]+df.soil[i, "thickness"], 3)
  }
  df.soil <- df.soil %>%
    dplyr::mutate(lower = upper+thickness) %>%
    dplyr::select(-thickness)

  # add rows in 1m depth
  if (!(100 %in% df.soil$lower)){
    cross_1m <- which(df.soil$lower>100 & df.soil$upper<100)
    df.soil <- rbind(df.soil[1:(cross_1m-1), ],
                     c(df.soil$mat[cross_1m], df.soil$lower[cross_1m-1], 100, as.character(df.soil$texture[cross_1m])),
                     c(df.soil$mat[cross_1m], 100, df.soil$lower[cross_1m], as.character(df.soil$texture[cross_1m])),
                     df.soil[(cross_1m+1):nrow(df.soil), ])
  }

  return(df.soil)
}
