
fnc_write_climdb(df.ids = test.ids.bds,
                 clim_dir = "./tmp/clim_files/",
                 mindate = as.Date("2000-01-01"),
                 maxdate = as.Date("2010-12-31"))

# set clim_args
test.ids.bds <- left_join(test.ids.bds, fnc_relateCoords(test.ids.bds)[c("ID_custom", "id_standard")])
clim_args <-
  lapply(split(test.ids.bds, seq(nrow(test.ids.bds))),
         function(x) list(IDs = as.character(x$ID_custom),
                          id_standard = x$id_standard,
                          clim_dir = "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/clim_files/"))
names(clim_args) <- as.character(test.ids.bds$ID_custom)

soil.test <- fnc_get_soil(df.ids = test.ids.bds,
                          soil_option = "BZE",
                          testgebiet = "BDS",
                          PTF_to_use = "HYPRES",
                          rootsmethod = "hartmann",
                          humus_roots = F)
parms.test <- fnc_get_params(df.ids = test.ids.bds,
                             tree_species = "spruce")

res <- run_multisite_LWFB90(options_b90 = set_optionsLWFB90(startdate = as.Date("2009-01-01"),
                                                            enddate = as.Date("2010-12-31"),
                                                            root_method = "soilvar"),
                            param_b90 = fnc_get_params(tree_species = "spruce",
                                                       df.ids = test.ids.bds),
                            soil = soil.test,

                            climate = fnc_read_climdb,
                            climate_args = clim_args,

                            cores = 5)

# single df
fnc_read_climdb(IDs = test.ids.bds[1, "ID_custom"],
                id_standard = test.ids.bds[1, "id_standard"],
                clim_dir = "H:/FVA-Projekte/P01540_WHHKW/Daten/Urdaten/test/clim_files/")

