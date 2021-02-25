df.ids <- test.ids.bds

ls.parms <- fnc_get_params(df.ids, tree_species = "spruce")
ls.soil <- fnc_get_soil(df.ids, soil_option = "BZE", testgebiet = "NPS", PTF_to_use = "HYPRES", limit_MvG = T, rootsmethod = "betamodel" )

options.b90 <- set_optionsLWFB90(startdate = as.Date("2008-01-01"),
                                 enddate = as.Date("2009-06-01"))


clim_args <-
  lapply(split(df.ids, seq(nrow(df.ids))),
         function(x) list(id = as.character(x$ID_custom),
                          easting = x$easting,
                          northing = x$northing,
                          mindate = options.b90$startdate,
                          maxdate = options.b90$enddate))
names(clim_args) <- as.character(df.ids$ID_custom)

res <- run_multisite_LWFB90(options_b90 = options.b90,
                            soil = ls.soil,
                            param_b90 = ls.parms,

                            climate = fnc_get_clim_sgl,
                            climate_args = clim_args)
