# set options
options.b90 <- set_optionsLWFB90(startdate = as.Date("2009-01-01"),
                                 enddate = as.Date("2010-12-31"),
                                 root_method = "soilvar")


# climate arguments for on-the-fly-processing
clim_args <- fnc_get_climate_args_poly(df.ids = shp.ids,
                                       mindate = options.b90$startdate,
                                       maxdate = options.b90$enddate)

# create soil
soil.test <- fnc_get_soil(df.ids = shp.ids,
                          soil_option = "STOK",
                          PTF_to_use = "WESSOLEK",
                          parallel_processing = F)


# create parameters
parms.test <- fnc_get_params(df.ids = shp.ids,
                             tree_species = "spruce")

# remove poins without soil information
with_soil_data <- which(shp.ids$ID_custom %in% names(soil.test))
parms.test <- parms.test[with_soil_data]
clim_args <- clim_args[with_soil_data]

res <- run_multisite_LWFB90(options_b90 = options.b90,
                            param_b90 = parms.test,
                            soil = soil.test,

                            climate = fnc_read_climdb_poly,
                            climate_args = clim_args,

                            rtrn_input = F,

                            cores = parallel::detectCores())

# single df
clim_args[[1]]
test <- fnc_read_climdb_poly(IDs = clim_args[[1]]$IDs,
                             clim_dir = clim_args[[1]]$clim_dir,
                             weights = clim_args[[1]]$weights,
                             mindate = clim_args[[1]]$mindate,
                             maxdate = clim_args[[1]]$maxdate)

