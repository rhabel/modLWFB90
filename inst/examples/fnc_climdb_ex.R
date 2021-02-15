# write project climate database from "2010-01-01" to "2011-12-31"
fnc_write_climdb(df.ids = test.ids.bds,
                 path_bdout = "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/")

# set clim_args
clim_args <-
  lapply(test.ids.bds$ID_custom,
         function(x) list(IDs = as.character(x),
                          path_climdb = "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/clim_data.sqlite"))
names(clim_args) <- as.character(test.ids.bds$ID_custom)

soil.test <- fnc_get_soil(df.ids = test.ids.bds,
                          soil_option = "BZE",
                          testgebiet = "BDS",
                          PTF_to_use = "HYPRES",
                          rootsmethod = "hartmann",
                          humus_roots = F)

res <- run_multisite_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2010-01-01"),
                                                            enddate = as.Date("2011-12-31"),
                                                            root_method = "soilvar"),
                            param_b90 = fnc_get_params(tree_species = "spruce",
                                                       df.ids = test.ids.bds),
                            soil = soil.test,
                            climate = fnc_read_climdb,
                            climate_args = clim_args)
