# set options
options.b90 <- set_optionsLWFB90(startdate = as.Date("2009-01-01"),
                                 enddate = as.Date("2010-12-31"),
                                 root_method = "soilvar")

# set clim_args
df.ids <- left_join(test_ids_bze, fnc_relateCoords(test_ids_bze)[c("ID_custom", "id_standard", "tranche")])

# climate arguments for on-the-fly-processing
climlist <- split(df.ids, seq(nrow(df.ids)))
names(climlist) <- as.character(df.ids$ID_custom)

clim_args <-
  lapply(climlist,
         function(x) list(IDs = as.character(x$ID_custom),
                          id_standard = x$id_standard,
                          clim_dir = paste0(input_clim, "tr", x$tranche, "/"),
                          mindate = options.b90$startdate,
                          maxdate = options.b90$enddate))

# create soil
soil.test <- fnc_get_soil(df.ids = test_ids_bze,
                          soil_option = "BZE",
                          PTF_to_use = "WESSOLEK")
# create parameters
parms.test <- fnc_get_params(df.ids = test_ids_bze,
                             tree_species = "spruce")

# remove poins without soil information
with_soil_data <- which(df.ids$ID_custom %in% names(soil.test))
parms.test <- parms.test[with_soil_data]
clim_args <- clim_args[with_soil_data]

res <- run_multisite_LWFB90(options_b90 = options.b90,
                            param_b90 = parms.test,
                            soil = soil.test,

                            climate = fnc_read_climdb,
                            climate_args = clim_args,

                            rtrn_input = F,

                            cores = parallel::detectCores())

# single df
clim_args[[1]]
test <- fnc_read_climdb(IDs = clim_args[[1]]$IDs,
                        id_standard = clim_args[[1]]$id_standard,
                        clim_dir = clim_args[[1]]$clim_dir,
                        mindate = clim_args[[1]]$mindate,
                        maxdate = clim_args[[1]]$maxdate)

