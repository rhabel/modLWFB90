#' Function to reduce data output from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it. This is what this function is made for. It writes .rds files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. \cr This "step-in-between" is necessary because SQLite does not support parallel writing.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param layercols a sting containing the desired output products. Full list of possible output columns can be find on the help page of \code{\link[LWFBrook90R]{run_LWFB90}} under \code{Layer outputs}
#' @param dailycols same as layercols but for daily output. For possible options see \code{\link[LWFBrook90R]{run_LWFB90}}
#' @param dir_name directory for tmp files, if \code{NA} (default), output will be returned to console
#'
#' @return writes output to
#' @example inst/examples/fnc_check_errors_ex.R
#'
#' @import data.table
#' @export

fnc_write <- function(x,
                      layercols = NA,
                      dailycols = NA,
                      dir_name = NA){

  # soil.df <- as.data.frame(ls.soil[[1]])
  # colnames(soil.df) <- tolower(colnames(soil.df))
  # id_run <- ls.soil[[1]]$ID_custom[1]
  # param_std <- param_b90

  # soil
  soil.df <- as.data.frame(get("soil", envir = parent.frame(3)))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]
  param_std <- get("param_b90", envir = parent.frame(3))


  if(!any(is.na(dailycols))){
    # daily
    daily <- x$daily_output[,..dailycols]
    daily[, "ID_custom" := id_run]
    daily[, "coords_x" := param_std$coords_x]
    daily[, "coords_y" := param_std$coords_y]
    setcolorder(daily,
                c(which(colnames(daily) == "ID_custom"),
                  which(colnames(daily) == "coords_x"),
                  which(colnames(daily) == "coords_y"),
                  which(!colnames(daily) %in% c("ID_custom","coords_x","coords_y"))))
  }

  # layer
  if(!any(is.na(layercols))){
    if(!all(c("nl", "yr", "mo", "da") %in% layercols)){
      notinlcol <- c("nl", "yr", "mo", "da")[!c("nl", "yr", "mo", "da") %in% layercols]
      layercols <- c(notinlcol, layercols)
    }
    layer <- x$layer_output[,..layercols]
    soil.cmp <- data.table::as.data.table(soil.df[c("nl", "upper", "lower")])
    #setkeys for joiningdata.table
    data.table::setkey(layer,nl)
    data.table::setkey(soil.cmp,nl)
    # join
    layer <- layer[soil.cmp]
    layer <- data.table::setorder(layer, yr, mo, da, nl)
    layer[, "ID_custom" := id_run]
    layer[, "coords_x" := param_std$coords_x]
    layer[, "coords_y" := param_std$coords_y]
    keep <- c("ID_custom", "coords_x","coords_y",
              "yr", "mo", "da", "nl","upper", "lower",
              colnames(layer)[which(!colnames(layer) %in% c("ID_custom", "coords_x","coords_y",
                                                      "yr", "mo", "da", "nl","upper", "lower"))])
    layer <- layer[, keep, with = FALSE]
  }

  ls.out <- list()
  if(is.na(dir_name)){
    if(!any(is.na(layercols))){
      ls.out <- append(ls.out, list(layer))
      names(ls.out)[length(ls.out)] <- "layer"
    }
    if(!any(is.na(dailycols))){
      ls.out <- append(ls.out, list(daily))
      names(ls.out)[length(ls.out)] <- "daily"
    }

    return(ls.out)
  }else{
    # write to tmp
    if(!any(is.na(dailycols))){

      if(!dir.exists(paste0(dir_name, "daily/"))){
        dir.create(paste0(dir_name, "daily/"), recursive = T)}

      saveRDS(daily,
              file = paste0(dir_name, "daily/", id_run, ".rds"))

    }

    if(!any(is.na(layercols))){

      if(!dir.exists(paste0(dir_name, "layer/"))){
        dir.create(paste0(dir_name, "layer/"), recursive = T)}

      saveRDS(layer,
              file = paste0(dir_name, "layer/", id_run, ".rds"))

    }
  }



}
