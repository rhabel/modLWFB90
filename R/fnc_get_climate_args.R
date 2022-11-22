#' Short function to create list of climate arguments
#'
#' When climate data, as needed in \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, isn't provided as a list (which takes up ram if too many points are modelled), but as a list of climate arguments to be read out by \code{\link{fnc_read_climdb}}, this function quickly creates the argument list.
#'
#'
#' @param df.ids data frame with \code{ID_custom}, \code{id_standard}, \code{tranche} (if \code{df.ids} is created by \code{\link{fnc_create_IDs}}, these columns are already there. Must otherwise be created with \code{\link{fnc_relateCoords}}), and \code{lower}
#' @param mindate used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$startdate}.
#' @param maxdate used for filtering climate data to certain dates. Date-object, recommended to set to \code{options.b90$enddate}.
#' @param store_as when the file should be stored somewhere, a complete path with file ending \code{".rds"} must be provided, else if \code{NA}, list will be returned to the console
#'
#' @return list of climate arguments as needed by \code{\link[LWFBrook90R]{run_multisite_LWFB90}}
#'

fnc_get_climate_args <- function(df.ids,
                                 mindate,
                                 maxdate,
                                 store_as = NA){

  # climate arguments for on-the-fly-processing
  climlist <- split(df.ids, seq(nrow(df.ids)))
  names(climlist) <- as.character(df.ids$ID_custom)

  clim_args <-
    lapply(climlist,
           function(x) list(IDs = as.character(x$ID_custom),
                            id_standard = x$id_standard,
                            clim_dir = paste0(path_clim, "tr", x$tranche, "/"),
                            mindate = mindate,
                            maxdate = maxdate))

  if(!is.na(store_as)){
    saveRDS(object = clim_args, file = store_as)
  }else{
    return(df.final)
  }
}
