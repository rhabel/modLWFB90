#' Function to reduce data output from LWFB90-Runs
#'
#' LWFBrook90 creates a lot of output files. In order to keep data storage to a minimum, both \code{\link[LWFBrook90R]{run_LWFB90}} and \code{\link[LWFBrook90R]{run_multisite_LWFB90}} provide an \code{output_fun} - argument that can be used to reduce the output and directly write it. This is what this function is made for. Since v.0.5.0 LWFBrook90R changed the output/default settings. \cr \code{\link{fnc_write}} reduces and aggregates all columns returned by \code{\link[LWFBrook90R]{run_LWFB90}} (see help page). \cr \code{fnc_write_agg}  enables an even more detailed selection of drought indices. See detail section for a full list. Because many indices are calculated, this function will be significantly slower, so check, if you can get the necessary information from the standard list of output variables. \cr Both functions write .rds files with the desired output for each point. \code{\link{fnc_write_to_sql}} can be used to convert these files into a SQLite-DB. \cr This "step-in-between" is necessary because SQLite does not support parallel writing. Processed can be all output-values listed in \code{\link[LWFBrook90R]{run_LWFB90}}.
#'
#' @param x one of the intermediate products of \code{\link[LWFBrook90R]{run_LWFB90}} or  \code{\link[LWFBrook90R]{run_multisite_LWFB90}}, which is further processed internally. Can't be adjusted.
#' @param out_tables which tables of the \code{\link[LWFBrook90R]{run_LWFB90}}- tables will information be required from:  \code{layer}, \code{outputs}, or \code{outputs_layer}
#' @param aggr_tp a string containing the desired aggregation time period. Can be \code{daily},  \code{monthly}, \code{vegper}, \code{yearly}, \code{complete} (average over complete time period modelled), or any combination of the four (i.e. \code{monthly_vegper}). The latter creates one result-table for each aggregation term  detectable within the string.
#' \cr If aggregation periods other than \code{daily} are selected, all unitless balance terms of the "Outputs"-table of \code{\link[LWFBrook90R]{run_LWFB90}} will be averaged using \code{mean} (i.e. relawat will be aggreated to "mean relawat within each years vegetation period"). All other terms will be summed up over the desired time period (i.e. rfal will be aggreated to "sum of rainfall within each years time period"). \cr
#' \cr If aggregation periods other than \code{daily} are selected, all wetnes-related terms of the "Layer"-table of \code{\link[LWFBrook90R]{run_LWFB90}} (swati, theta, wetnes and psimi) will be averaged using \code{mean} (i.e. relawat will be aggreated to "mean relawat within each years vegetation period in the respective layer"). All flow-related terms will be summed up over the desired time period (i.e. vrfl will be aggreated to "sum of vertical flow within each years time period").
#' @param col_select_outputs a string containing the desired columns from the "outputs"-outputs-list of \code{\link[LWFBrook90R]{run_LWFB90}}. BETTER MAKE A SELECTION or RAM and result-size will go through the roof! Plus be honest - you won't need everything.
#' @param col_select_layer a string containing the desired columns from the "layer"-outputs-list of \code{\link[LWFBrook90R]{run_LWFB90}}. BETTER MAKE A SELECTION or RAM and result-size will go through the roof! Plus be honest - you won't need everything
#' @param depths dephts for which layer-discrete results shall be processed. Must be a vector of depths in cm (i.e. \code{ c(10, 30, 60) }). If kept at \code{NA}, all depths will be processed, so better make a selection. If the respective soil data frame isn't deep enough to get all data requested here, all valid depths will be created.
#'
#' @param dir_name directory for tmp files, if \code{NA} as in default, results are returned to console
#' @param soil_nm name of the modelled site. If the function is used in \code{\link[LWFBrook90R]{run_multisite_LWFB90}} will be processed internally. Must be provided if only one site is modelled with  \code{\link[LWFBrook90R]{run_LWFB90}}. This function argument will set the name of the result-files.
#'
#' @return Returns the desired output to .rds files, or, the console
#'
#' @section Output column selection:
#' IT IS HIGHLY RECOMMENDED TO MAKE A SUBSELECTION, OR THERE WILL BE A LOT OF COLUMNS.
#' For a complete list of possible output types plus description, see the details sections of \code{\link[LWFBrook90R]{run_LWFB90}}
#'
#' @example inst/examples/fnc_write_ex.R
#'
#' @import data.table
#' @export

fnc_write <- function(x,

                      aggr_tp,    # aggr_tp = c("daily_monthly_vegper_yearly_complete")
                      out_tables, # out_tables = c("outputs_layer")

                      col_select_outputs = NA, # col_select_outputs =  c("yr", "mo", "da", "doy", "stres", "relawat", "vrfln")
                      col_select_layer = NA, # col_select_layer = c("yr", "mo", "da", "doy", "nl", "theta", "swati", "tran")

                      depths = NA,# depths = c(10,30,600)

                      dir_name = NA,
                      soil_nm # soil_nm = "test"

                      ){

  # # soil
  soil.df <- as.data.frame(get("soil", envir = parent.frame(3)))
  soil.df$nl <- 1:nrow(soil.df)
  #id_num <- soil.df$ID[1]

  # Daily output ---------------------------------------- ####
  if(stringr::str_detect(out_tables, "outputs")){
    outputs <- x$output

    # select columns ----
    if(!any(is.na(col_select_outputs))){
      if(!all(c("yr", "mo", "da", "doy") %in% col_select_outputs )){
        col_select_outputs <- c(c("yr", "mo", "da", "doy")[!c("yr", "mo", "da", "doy") %in% col_select_outputs],
                                col_select_outputs)
      }
      outputs <- outputs[, ..col_select_outputs]
    }

    # aggregate ----
    if(stringr::str_detect(aggr_tp, "daily")){

      outputs_day <- data.table::copy(outputs)

      outputs_day[,"ID_custom" := soil_nm]
      cols <- union("ID_custom", names(outputs_day))
      outputs_day <- outputs_day[, ..cols]

    }

    if(stringr::str_detect(aggr_tp, "monthly")){

      # define empty data.tables:
      outputs_mean <- data.table::data.table()
      outputs_sum <- data.table::data.table()

      # for results with keys yr and mo
      outputs_mon <- data.table::data.table("yr" = numeric(0), "mo"= numeric(0))
      data.table::setkey(outputs_mon)

      # caclualte mean
      if(any(c("nits", "relawat", "safrac", "stres") %in% colnames(outputs))){
        cols <- c("nits", "relawat", "safrac", "stres")[c("nits", "relawat", "safrac", "stres") %in% colnames(outputs)]
        cols <- cols[order(match(cols,colnames(outputs)))]
        outputs_mean <- outputs[, lapply(.SD, mean),
                                .SDcols = cols,
                                by = list(yr, mo)]
        data.table::setkeyv(outputs_mean, c("yr", "mo"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(outputs), c("da", "doy", "yr", "mo", "nits", "relawat", "safrac", "stres"))
      if(length(cols)>0){
        outputs_sum <- outputs[, lapply(.SD, sum),
                               .SDcols = cols,
                               by = list(yr, mo)]
        data.table::setkeyv(outputs_sum, c("yr", "mo"))
      }

      # join results tables
      if(nrow(outputs_mean)>0){
        outputs_mon <- outputs_mon[outputs_mean]
      }
      if(nrow(outputs_sum)>0){
        outputs_mon <- outputs_mon[outputs_sum]
      }

      outputs_mon[,"ID_custom" := soil_nm]
      cols <- union("ID_custom", names(outputs_mon))
      outputs_mon <- outputs_mon[, ..cols]
    }

    if(stringr::str_detect(aggr_tp, "vegper")){
      # get beginning and end of growing season from input parameters
      vpstart <- x$model_input$param_b90$budburstdoy
      vpend <- x$model_input$param_b90$leaffalldoy
      dt.vp <- data.table::data.table(yr = unique(outputs$yr), vpstart, vpend)

      outputs_vp <- merge(outputs,
                          dt.vp, by = "yr")


      # define empty data.table::data.tables:
      outputs_mean <- data.table::data.table()
      outputs_sum <- data.table::data.table()

      # for results with keys yr and mo
      outputs_vp_out <- dt.vp
      data.table::setkey(outputs_vp_out, "yr")

      # caclualte mean
      if(any(c("nits", "relawat", "safrac", "stres") %in% colnames(outputs))){
        cols <- c("nits", "relawat", "safrac", "stres")[c("nits", "relawat", "safrac", "stres") %in% colnames(outputs)]
        cols <- cols[order(match(cols,colnames(outputs)))]
        outputs_mean <- outputs_vp[doy >= vpstart & doy <= vpend,
                                   lapply(.SD, mean),
                                   .SDcols = cols,
                                   by = yr]
        data.table::setkeyv(outputs_mean, c("yr"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(outputs), c("da", "doy", "yr", "mo", "nits", "relawat", "safrac", "stres"))
      if(length(cols)>0){
        outputs_sum <- outputs_vp[doy >= vpstart & doy <= vpend,
                                  lapply(.SD, sum),
                                  .SDcols = cols,
                                  by = yr]
        data.table::setkeyv(outputs_sum, c("yr"))
      }

      # join results tables
      if(nrow(outputs_mean)>0){
        outputs_vp_out <- outputs_vp_out[outputs_mean]
      }
      if(nrow(outputs_sum)>0){
        outputs_vp_out <- outputs_vp_out[outputs_sum]
      }

      outputs_vp_out[,"ID_custom" := soil_nm]
      cols <- union("ID_custom", names(outputs_vp_out))
      outputs_vp_out <- outputs_vp_out[, ..cols]
    }

    if(stringr::str_detect(aggr_tp, "yearly")){

      # define empty data.tables:
      outputs_mean <- data.table::data.table()
      outputs_sum <- data.table::data.table()

      # for results with keys yr and mo
      outputs_yr <- data.table::data.table("yr" = numeric(0))
      data.table::setkey(outputs_yr)

      # caclualte mean
      if(any(c("nits", "relawat", "safrac", "stres") %in% colnames(outputs))){
        cols <- c("nits", "relawat", "safrac", "stres")[c("nits", "relawat", "safrac", "stres") %in% colnames(outputs)]
        cols <- cols[order(match(cols,colnames(outputs)))]

        outputs_mean <- outputs[, lapply(.SD, mean),
                                .SDcols = cols,
                                by = yr]
        data.table::setkey(outputs_mean, "yr")
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(outputs), c("da", "doy", "yr", "mo", "nits", "relawat", "safrac", "stres"))
      if(length(cols)>0){
        outputs_sum <- outputs[, lapply(.SD, sum),
                               .SDcols = cols,
                               by = yr]
        data.table::setkey(outputs_sum, "yr")
      }

      # join results tables
      if(nrow(outputs_mean)>0){
        outputs_yr <- outputs_yr[outputs_mean]
      }
      if(nrow(outputs_sum)>0){
        outputs_yr <- outputs_yr[outputs_sum]
      }

      outputs_yr[,"ID_custom" := soil_nm]
      cols <- union("ID_custom", names(outputs_yr))
      outputs_yr <- outputs_yr[, ..cols]

    }

    if(stringr::str_detect(aggr_tp, "complete")){

      # define empty data.tables:
      outputs_mean <- data.table::data.table()
      outputs_sum <- data.table::data.table()

      # for results with keys yr and mo
      outputs_cmp <- data.table::data.table("ID_custom" = soil_nm)
      data.table::setkey(outputs_cmp)

      # caclualte mean
      if(any(c("nits", "relawat", "safrac", "stres") %in% colnames(outputs))){
        cols <- c("nits", "relawat", "safrac", "stres")[c("nits", "relawat", "safrac", "stres") %in% colnames(outputs)]
        cols <- cols[order(match(cols,colnames(outputs)))]

        outputs_mean <- outputs[, lapply(.SD, mean),
                                .SDcols = cols]
        outputs_mean[,"ID_custom" := soil_nm]
        data.table::setkey(outputs_mean, "ID_custom")
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(outputs), c("da", "doy", "yr", "mo", "nits", "relawat", "safrac", "stres"))
      if(length(cols)>0){
        outputs_sum <- outputs[, lapply(.SD, sum),
                               .SDcols = cols]
        outputs_sum[,"ID_custom" := soil_nm]
        data.table::setkey(outputs_sum, "ID_custom")
      }

      # join results tables
      if(nrow(outputs_mean)>0){
        outputs_cmp <- outputs_cmp[outputs_mean]
      }
      if(nrow(outputs_sum)>0){
        outputs_cmp <- outputs_cmp[outputs_sum]
      }

    }

  }

  # Layer output ---------------------------------------- ####
  if(stringr::str_detect(out_tables, "layer")){
    layer <- x$layer_output

    if(!any(is.na(col_select_layer))){

      if(!all(c("yr", "mo", "da", "doy", "nl") %in% col_select_layer )){
        col_select_layer <- c(col_select_layer,
                              c("yr", "mo", "da","doy",  "nl")[!c("yr", "mo", "da", "doy", "nl") %in% col_select_layer])
      }

      layer <- layer[, ..col_select_layer]
    }


    # select depths/layer ----
    if(!any(is.na(depths))){

      depths <- depths[depths < max(soil.df$lower*-100)]

      depths_fin <- numeric(length(depths))
      for(i in 1:length(depths)){
        depths_fin[i] <- which(soil.df$lower <= depths[i]*-0.01)[1]
      }

      layer <- layer[nl %in% depths_fin]
    }else{
      depths_fin <- 1:nrow(soil.df)
    }

    depth_reference <- soil.df[soil.df$nl %in% depths_fin, c("nl","upper", "lower")]
    depth_reference$ID_custom = soil_nm
    depth_reference <- depth_reference[,c("ID_custom", "nl", "upper", "lower")]
    depth_reference <- data.table::as.data.table(depth_reference)


    # aggregate ----
    if(stringr::str_detect(aggr_tp, "daily")){

      layer_day <- data.table::copy(layer)

      layer_day[,"ID_custom" := soil_nm]
      cols <- union(c("ID_custom", "yr", "mo", "da", "doy", "nl"), names(layer_day))
      layer_day <- layer_day[, ..cols]

    }

    if(stringr::str_detect(aggr_tp, "monthly")){

      # define empty data.tables:
      layer_mean <- data.table::data.table()
      layer_sum <- data.table::data.table()

      # for results with keys yr and mo
      layer_mon <- data.table::data.table("yr" = numeric(0), "mo"= numeric(0), "nl" = numeric(0))
      data.table::setkey(layer_mon)

      # caclualte mean
      if(any(c("swati", "theta", "wetnes", "psimi") %in% colnames(layer))){
        cols <- c("swati", "theta", "wetnes", "psimi")[c("swati", "theta", "wetnes", "psimi") %in% colnames(layer)]
        cols <- cols[order(match(cols,colnames(layer)))]

        layer_mean <- layer[, lapply(.SD, mean),
                                .SDcols = cols,
                                by = list(yr, mo, nl)]
        data.table::setkeyv(layer_mean, c("yr", "mo", "nl"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(layer), c("da", "doy", "yr", "mo", "nl", "swati", "theta", "wetnes", "psimi"))
      if(length(cols)>0){
        layer_sum <- layer[, lapply(.SD, sum),
                               .SDcols = cols,
                               by = list(yr, mo, nl)]
        data.table::setkeyv(layer_sum, c("yr", "mo", "nl"))
      }

      # join results tables
      if(nrow(layer_mean)>0){
        layer_mon <- layer_mon[layer_mean]
      }
      if(nrow(layer_sum)>0){
        layer_mon <- layer_mon[layer_sum]
      }

      layer_mon[,"ID_custom" := soil_nm]
      cols <- union(c("ID_custom", "yr", "mo", "nl"), names(layer_mon))
      layer_mon <- layer_mon[, ..cols]
    }

    if(stringr::str_detect(aggr_tp, "vegper")){
      # get beginning and end of growing season from input parameters
      vpstart <- x$model_input$param_b90$budburstdoy
      vpend <- x$model_input$param_b90$leaffalldoy
      dt.vp <- data.table::data.table(yr = unique(layer$yr), vpstart, vpend)

      layer_vp <- merge(layer,
                          dt.vp, by = "yr")


      # define empty data.tables:
      layer_mean <- data.table::data.table()
      layer_sum <- data.table::data.table()

      # for results with keys yr and mo
      layer_vp_out <- data.table::data.table(yr = expand.grid(unique(layer$yr), depths_fin)$Var1,
                                 nl = expand.grid(unique(layer$yr), depths_fin)$Var2,
                                 vpstart, vpend)
      data.table::setkeyv(layer_vp_out, c("yr", "nl"))

      # caclualte mean
      if(any(c("swati", "theta", "wetnes", "psimi") %in% colnames(layer))){
        cols <- c("swati", "theta", "wetnes", "psimi")[c("swati", "theta", "wetnes", "psimi") %in% colnames(layer)]
        cols <- cols[order(match(cols,colnames(layer)))]

        layer_mean <- layer_vp[doy >= vpstart & doy <= vpend,
                                   lapply(.SD, mean),
                                   .SDcols = cols,
                                   by = list(yr, nl)]
        data.table::setkeyv(layer_mean, c("yr", "nl"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(layer), c("da", "doy", "yr", "mo", "nl", "swati", "theta", "wetnes", "psimi"))
      if(length(cols)>0){
        layer_sum <- layer_vp[doy >= vpstart & doy <= vpend,
                                  lapply(.SD, sum),
                                  .SDcols = cols,
                              by = list(yr, nl)]
        data.table::setkeyv(layer_sum, c("yr", "nl"))
      }

      # join results tables
      if(nrow(layer_mean)>0){
        layer_vp_out <- layer_vp_out[layer_mean]
      }
      if(nrow(layer_sum)>0){
        layer_vp_out <- layer_vp_out[layer_sum]
      }

      layer_vp_out[,"ID_custom" := soil_nm]
      cols <- union(c("ID_custom", "yr", "vpstart", "vpend", "nl"), names(layer_vp_out))
      layer_vp_out <- layer_vp_out[, ..cols]
    }

    if(stringr::str_detect(aggr_tp, "yearly")){

      # define empty data.tables:
      layer_mean <- data.table::data.table()
      layer_sum <- data.table::data.table()

      # for results with keys yr and mo
      layer_yr <- data.table::data.table("yr" = numeric(0), "nl" = numeric(0))
      data.table::setkey(layer_yr)

      # caclualte mean
      if(any(c("swati", "theta", "wetnes", "psimi") %in% colnames(layer))){
        cols <- c("swati", "theta", "wetnes", "psimi")[c("swati", "theta", "wetnes", "psimi") %in% colnames(layer)]
        cols <- cols[order(match(cols,colnames(layer)))]

        layer_mean <- layer[, lapply(.SD, mean),
                                .SDcols = cols,
                            by = list(yr, nl)]
        data.table::setkeyv(layer_mean, c("yr", "nl"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(layer), c("da", "doy", "yr", "mo", "nl", "swati", "theta", "wetnes", "psimi"))
      if(length(cols)>0){
        layer_sum <- layer[, lapply(.SD, sum),
                               .SDcols = cols,
                           by = list(yr, nl)]
        data.table::setkeyv(layer_sum, c("yr", "nl"))
      }

      # join results tables
      if(nrow(layer_mean)>0){
        layer_yr <- layer_yr[layer_mean]
      }
      if(nrow(layer_sum)>0){
        layer_yr <- layer_yr[layer_sum]
      }


      layer_yr[,"ID_custom" := soil_nm]
      cols <- union(c("ID_custom", "yr", "nl"), names(layer_yr))
      layer_yr <- layer_yr[, ..cols]

    }

    if(stringr::str_detect(aggr_tp, "complete")){

      # define empty data.tables:
      layer_mean <- data.table::data.table()
      layer_sum <- data.table::data.table()

      # for results with keys yr and mo
      layer_cmp <- data.table::data.table("ID_custom" = character(0), "nl" = numeric(0))
      data.table::setkey(layer_cmp)

      # caclualte mean
      if(any(c("swati", "theta", "wetnes", "psimi") %in% colnames(layer))){
        cols <- c("swati", "theta", "wetnes", "psimi")[c("swati", "theta", "wetnes", "psimi") %in% colnames(layer)]
        cols <- cols[order(match(cols,colnames(layer)))]

        layer_mean <- layer[, lapply(.SD, mean),
                                .SDcols = cols,
                            by = nl]
        layer_mean[,"ID_custom" := soil_nm]
        data.table::setkeyv(layer_mean, c("ID_custom", "nl"))
      }

      # calculate sum
      # is any "summable" term requested?
      cols <- setdiff(names(layer), c("da", "doy", "yr", "mo", "nl", "swati", "theta", "wetnes", "psimi"))
      if(length(cols)>0){
        layer_sum <- layer[, lapply(.SD, sum),
                               .SDcols = cols,
                           by = nl]
        layer_sum[,"ID_custom" := soil_nm]
        data.table::setkeyv(layer_sum, c("ID_custom", "nl"))
      }

      # join results tables
      if(nrow(layer_mean)>0){
        layer_cmp <- layer_cmp[layer_mean]
      }
      if(nrow(layer_sum)>0){
        layer_cmp <- layer_cmp[layer_sum]
      }

    }

  }


  # create and save/return outputlist -------------------- ####

  if(stringr::str_detect(out_tables, "outputs")){

    ls.outputs <- list()

    if(stringr::str_detect(aggr_tp, "daily")){
      ls.outputs <- append(ls.outputs, list(outputs_day))
      names(ls.outputs)[length(ls.outputs)] <- "outputs_daily"
    }
    if(stringr::str_detect(aggr_tp, "monthly")){
      ls.outputs <- append(ls.outputs, list(outputs_mon))
      names(ls.outputs)[length(ls.outputs)] <- "outputs_monthly"
    }
    if(stringr::str_detect(aggr_tp, "vegper")){
      ls.outputs <- append(ls.outputs, list(outputs_vp_out))
      names(ls.outputs)[length(ls.outputs)] <- "outputs_vegper"
    }
    if(stringr::str_detect(aggr_tp, "yearly")){
      ls.outputs <- append(ls.outputs, list(outputs_yr))
      names(ls.outputs)[length(ls.outputs)] <- "outputs_yearly"
    }
    if(stringr::str_detect(aggr_tp, "complete")){
      ls.outputs <- append(ls.outputs, list(outputs_cmp))
      names(ls.outputs)[length(ls.outputs)] <- "outputs_complete"
    }

  }

  if(stringr::str_detect(out_tables, "layer")){

    ls.layer <- list()

    if(stringr::str_detect(aggr_tp, "daily")){
      ls.layer <- append(ls.layer, list(layer_day))
      names(ls.layer)[length(ls.layer)] <- "layer_daily"
    }
    if(stringr::str_detect(aggr_tp, "monthly")){
      ls.layer <- append(ls.layer, list(layer_mon))
      names(ls.layer)[length(ls.layer)] <- "layer_monthly"
    }
    if(stringr::str_detect(aggr_tp, "vegper")){
      ls.layer <- append(ls.layer, list(layer_vp_out))
      names(ls.layer)[length(ls.layer)] <- "layer_vegper"
    }
    if(stringr::str_detect(aggr_tp, "yearly")){
      ls.layer <- append(ls.layer, list(layer_yr))
      names(ls.layer)[length(ls.layer)] <- "layer_yearly"
    }
    if(stringr::str_detect(aggr_tp, "complete")){
      ls.layer <- append(ls.layer, list(layer_cmp))
      names(ls.layer)[length(ls.layer)] <- "layer_complete"
    }

    ls.layer <- append(ls.layer, list(depth_reference))
    names(ls.layer)[length(ls.layer)] <- "df_depth_reference"

  }

  # put together
  if(stringr::str_detect(out_tables, "layer") & stringr::str_detect(out_tables, "outputs")){
    ls.out <- list("outputs"= ls.outputs, "layer" = ls.layer)
  }else if(stringr::str_detect(out_tables, "layer") & !stringr::str_detect(out_tables, "outputs")){
    ls.out <- list("layer" = ls.layer)
  }else if(!stringr::str_detect(out_tables, "layer") & stringr::str_detect(out_tables, "outputs")){
    ls.out <- list("outputs"= ls.outputs)
  }

  # write or return ----------------------------------------------- ####
  if(is.na(dir_name)){

    return(ls.out)

  }else{

    if(stringr::str_detect(out_tables, "layer")){


      if(!dir.exists(paste0(dir_name, "layer/depth_reference/"))){
        dir.create(paste0(dir_name, "layer/depth_reference/"), recursive = T)}

      saveRDS(depth_reference, file = paste0(dir_name, "layer/depth_reference/", soil_nm, ".rds"))


      # write to tmp
      if(stringr::str_detect(aggr_tp, "daily")){

        if(!dir.exists(paste0(dir_name, "layer/daily/"))){
          dir.create(paste0(dir_name, "layer/daily/"), recursive = T)}

        saveRDS(layer_day, file = paste0(dir_name, "layer/daily/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "monthly")){

        if(!dir.exists(paste0(dir_name, "layer/monthly/"))){
          dir.create(paste0(dir_name, "layer/monthly/"), recursive = T)}

        saveRDS(layer_mon, file = paste0(dir_name, "layer/monthly/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "vegper")){

        if(!dir.exists(paste0(dir_name, "layer/vegper/"))){
          dir.create(paste0(dir_name, "layer/vegper/"), recursive = T)}

        saveRDS(layer_vp_out, file = paste0(dir_name, "layer/vegper/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "yearly")){

        if(!dir.exists(paste0(dir_name, "layer/yearly/"))){
          dir.create(paste0(dir_name, "layer/yearly/"), recursive = T)}

        saveRDS(layer_yr, file = paste0(dir_name, "layer/yearly/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "complete")){

        if(!dir.exists(paste0(dir_name, "layer/complete/"))){
          dir.create(paste0(dir_name, "layer/complete/"), recursive = T)}

        saveRDS(layer_cmp, file = paste0(dir_name, "layer/complete/", soil_nm, ".rds"))

      }
    }
    if(stringr::str_detect(out_tables, "outputs")){
      # write to tmp
      if(stringr::str_detect(aggr_tp, "daily")){

        if(!dir.exists(paste0(dir_name, "outputs/daily/"))){
          dir.create(paste0(dir_name, "outputs/daily/"), recursive = T)}

        saveRDS(outputs_day, file = paste0(dir_name, "outputs/daily/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "monthly")){

        if(!dir.exists(paste0(dir_name, "outputs/monthly/"))){
          dir.create(paste0(dir_name, "outputs/monthly/"), recursive = T)}

        saveRDS(outputs_mon, file = paste0(dir_name, "outputs/monthly/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "vegper")){

        if(!dir.exists(paste0(dir_name, "outputs/vegper/"))){
          dir.create(paste0(dir_name, "outputs/vegper/"), recursive = T)}

        saveRDS(outputs_vp_out, file = paste0(dir_name, "outputs/vegper/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "yearly")){

        if(!dir.exists(paste0(dir_name, "outputs/yearly/"))){
          dir.create(paste0(dir_name, "outputs/yearly/"), recursive = T)}

        saveRDS(outputs_yr, file = paste0(dir_name, "outputs/yearly/", soil_nm, ".rds"))

      }

      if(stringr::str_detect(aggr_tp, "complete")){

        if(!dir.exists(paste0(dir_name, "outputs/complete/"))){
          dir.create(paste0(dir_name, "outputs/complete/"), recursive = T)}

        saveRDS(outputs_cmp, file = paste0(dir_name, "outputs/complete/", soil_nm, ".rds"))

      }
    }

  }
}

