#' Creating soil dataframes from BZE Data
#'
#' This function has been merged from several steps of Paul Schmidt-Walter's documentation on soil parametrisation from BZE data ("U:\\Brook90_2018\\paul_schmidt_walter_2018\\Dokumentation\\2_Bodenparameter.nb".). In short, it is the equivalent to fnc_depth_disc, just for the data structure of the data extracted from the BZE-rasterstack using data.table and some merging and melting steps.
#'
#' @param soil soil information in a certain form
#' @param skltn discretisation information in a certain form
#'
#'
#' @return Returns a longer data.table that already includes an earlier version of ls.soils. Further processed in \code{fnc_soil_bze( )}.

fnc_MakeSoil_BZE <- function(soil, skltn){

  limits <- soil[,list(ID, humus, bodtief, slope, aspect, WugebNr)]
  layers <- soil[,-which(names(soil) %in% c("humus", "bodtief", "slope", "aspect", "WugebNr")), with=F]

  #Layers umformen nach lang
  layers_m <- melt(data.table(layers), id.vars = "ID")

  #extract depth from variable-column (0-4)
  layers_m[, depth := gsub("[^\\d]+", "", as.character(variable), perl=TRUE)]

  #remove depth from varnames
  layers_m[, variable := gsub("[0-9]", "", as.character(variable), perl=TRUE)]
  #make layers wide again, variables to column by depth
  layers_wide <- dcast(layers_m, ID+depth~variable)

  layers_wide[, lower := ifelse(depth == "0", 5,
                                ifelse(depth =="1", 10,
                                       ifelse(depth =="2",30,
                                              ifelse(depth =="3", 60, max(skltn$lower)))))]

  #breite layers wieder mit den limits und infos verschneiden
  setkey(layers_wide, ID)
  limits[,profile_top := humus]

  limits[,roots_bottom := bodtief]
  limits[,roots_bottom_rnd := ifelse(roots_bottom<10, 10,5*ceiling(roots_bottom/5))] # auf 5 cm aufrunden, mindestens 10 cm

  #merge limits/info back in
  lay_lim <- layers_wide[limits[,.(ID, profile_top, roots_bottom, roots_bottom_rnd, slope, aspect, WugebNr)]]
  lay_lim[, upper := c(0,lower[1:4]), by= ID]


  # Interval-Merge der Tiefenstufen mit der B90-Schichtdiskretisierung -> Tabelle aufblasen, lower/upper anpassen
  # prolong the lower soil depth to roots_bottom
  layerthickness <- function(laytop, laybot, upperlim, lowerlim){

    thickness <- (ifelse(laybot<lowerlim & laybot != max(laybot),laybot,lowerlim) -
                    ifelse(laytop<upperlim & laybot>upperlim,upperlim,
                           ifelse(laybot<upperlim,0,laytop)) ) * (laybot>upperlim & laytop<lowerlim) ## letzer term nicht >= // <= ?? pr?fen!!
    return(thickness)

  }

  #upper, lower layer bounds

  lay_lim[,thick := layerthickness(upper, lower, 0, max(c(roots_bottom_rnd, max(skltn$lower)))), by= ID]
  lay_lim[,c("upper_new","lower_new") := list(c(0,cumsum(thick[1:length(thick)-1])),
                                              cumsum(thick)), by=ID]
  lay_lim[,c("thick", "upper", "lower") := NULL]

  setnames(lay_lim, c("upper_new", "lower_new"), c("upper", "lower"))
  setkey(lay_lim, upper, lower)

  # Interval merge: model-nodes x layers.

  if(any(!complete.cases(lay_lim))){
    completelay_lim <- lay_lim[complete.cases(lay_lim)]
    lay_long <- data.table::foverlaps(skltn, completelay_lim, type = "within", nomatch=0L)

    lay_long <- rbind(lay_long, cbind(lay_lim[!complete.cases(lay_lim)],
                                      "i.upper" = NA,
                                      "i.lower" = NA))
  }else{
    lay_long <- data.table::foverlaps(skltn, lay_lim, type = "within", nomatch=0L)
  }

  setkey(lay_long, ID, i.upper)
  #setkey(lay_long, id)
  return(lay_long)
}
