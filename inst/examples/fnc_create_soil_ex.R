test.ids.bds <- data.frame("ID" = c(1,2, 3, 4, 5),
                           "easting" = c(493497, 503000, 515138, 501000, 503330),
                           "northing" = c(5281811, 5292500, 5289355, 5293600, 5291700))

# Leitprofil-db:
df.LEIT.BDS <- readxl::read_excel(paste0(input_stok, "testregionen/20200129_StokDB_Leitprofile_705.xlsx"),
                          sheet = "LEITPROFIL")

df.LEIT.NPS <- readxl::read_excel(paste0(input_stok, "testregionen/20200109_StokDB_Leitprofile_NLP.xlsx"),
                          sheet = "LEITPROFIL")

# STOKA - shapefiles
sf.STOK.BDS <- sf::st_read(dsn = paste0(input_gis, "Testgebiete/BDS/BDS_STOKA_Clip_UTM.shp"))
sf.STOK.NPS <- sf::st_read(dsn = paste0(input_gis, "Testgebiete/NPS/NP_STOKA_Clip_UTM.shp"))

fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "STOK",
                testgebiet = "BDS",
                PTF_to_use = "HYPRES",
                rootsmethod = "hartmann",
                humus_roots = F)
fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "STOK",
                testgebiet = "BDS",
                PTF_to_use = "HYPRES",
                rootsmethod = "betamodel",
                beta = 0.95)
fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "BZE",
                testgebiet = "BDS",
                PTF_to_use = "HYPRES",
                rootsmethod = "betamodel",
                beta = 0.95)


# sample data frame with "own" data
df.own.test <- data.frame("ID" = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)),
                          "mat" = rep(1:4, 5),
                          "upper" = c(0,5,10,30, 0,10,50,60, 0,5,10,30, 0,10,50,60, 0,15,20,70),
                          "lower" = c(5,10,30,60, 10,50,60,100, 5,10,30,60, 10,50,60,100, 15,20,70,100),
                          "sand" = rep(30, 20),
                          "silt" = rep(40, 20),
                          "clay" = rep(30, 20),
                          "oc.pct" = c(5,2,1,0,4,1,0,0, 5,2,1,0,4,1,0,0,8,2,0,0),
                          "bd" = c(1.1,1.2,1.4,1.5,1,1.2, 1.2, 1.3, 1.1,1.2,1.4,1.5,1,1.2, 1.2, 1.3, 1.1,1.4, 1.6, 1.6 ),
                          "gravel" = c(1,5,5,20, 0,0,10,15, 5,8,8,10, 20,20,25,60,0,0,0,0) ,
                          "humus" = c(rep(0.04,4), rep(0.06, 4), rep(0.04,4), rep(0.1, 4), rep(0.05, 4)))

fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "OWN",
                testgebiet = "BDS",
                PTF_to_use = "HYPRES",
                df.soils = df.own.test,
                rootsmethod = "betamodel",
                beta = 0.95)
