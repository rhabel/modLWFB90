#' Function to reduce data output from automated LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it. This is what this function is made for. It writes .rds files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. \cr This "step-in-between" is necessary because SQLite does not support parallel writing.
#'
#' @param x one of the intermediate producs of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param layercols a sting containing the desired output products. Full list of possible output columns can be find on the help page of \code{\link[LWFBrook90R]{run_LWFB90}} under \code{Layer outputs}
#' @param dailycols same as layercols but for daily output. For possible options see \code{\link[LWFBrook90R]{run_LWFB90}}
#' @param dir_name directory for tmp files
#'
#' @return writes output to
#' @example inst/examples/fnc_check_errors_ex.R
#'
#' @import data.table
#' @export

fnc_write <- function(x,
                      dailycols = NA,
                      layercols = NA,
                      dir_name = NA){

  # soil.df <- ls.soils[[1]]
  # colnames(soil.df) <- tolower(colnames(soil.df))
  # id_run <- ls.soils[[1]]$ID_custom[1]
  # param_std <- param_b90

  # soil
  soil.df <- get("soil", envir = parent.frame(3))
  id_run <- get("soil", envir = parent.frame(3))$id_custom[1]

  if(!any(is.na(dailycols))){
    # daily
    daily <- x$daily_output[,..dailycols]
    daily[, "ID_custom" := id_run]
    setcolorder(daily, c(which(colnames(daily) == "ID_custom"), which(colnames(daily) != "ID_custom")))
  }

  # layer
  if(!any(is.na(layercols))){
    layer <- x$layer_output[,..layercols]
    soil.cmp <- data.table::as.data.table(soil.df[c("nl", "upper", "lower")])
    #setkeys for joiningdata.table
    data.table::setkey(layer,nl)
    data.table::setkey(soil.cmp,nl)
    # join
    layer <- layer[soil.cmp]
    layer <- data.table::setorder(layer, yr, mo, da, nl)
    layer[, "ID_custom" := id_run]
    keep <- c("ID_custom",  layercols[which(layercols %in% c("yr", "mo", "da", "nl"))],
              "upper", "lower", layercols[which(!layercols %in% c("yr", "mo", "da", "nl"))])
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
