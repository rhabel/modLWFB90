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
                PTF = "HYPRES",
                rootsmethod = "hartmann",
                humus_roots = F)
fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "STOK",
                testgebiet = "BDS",
                PTF = "HYPRES",
                rootsmethod = "betamodel",
                beta = 0.95)
fnc_create_soil(df.ids = test.ids.bds,
                soil_option = "BZE",
                testgebiet = "BDS",
                PTF = "HYPRES",
                rootsmethod = "betamodel",
                beta = 0.95)
