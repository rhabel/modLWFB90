### Tree species' parametrisation as agreed upon in WHH-KW

# beech -------- ####
params_beech <- LWFBrook90R::set_paramLWFB90(zw = 10,
                                             obsheight = 0.02,
                                             dswmax = 0.5,
                                             dpsimax = 0.01,
                                             winlaifrac = 0,

                                             budburst_species = "Fagus sylvatica",

                                             alb = 0.21,
                                             albsn = 0.47,
                                             lwidth = 0.04,
                                             rhotp = 2,
                                             #glmax = 0.00453,
                                             glmax = 0.006,
                                             radex = 0.59,
                                             glmin = 0.0002,
                                             mxkpl = 8,
                                             maxrlen = 3200,
                                             psicr = -2,
                                             rrad = 0.25,

                                             cintrl = 0.6,
                                             cintrs = 0.5,
                                             cintsl = 2.4,
                                             cintss = 2,
                                             frintlai = 0.08,
                                             frintsai = 0.08,
                                             fsintlai = 0.08,
                                             fsintsai = 0.4,

                                             maxlai = 6,
                                             sai = 1,
                                             height = 34,
                                             age_ini = 100,
                                             maxrootdepth = -1.6,
                                             betaroot = 0.966)

# oak ---------- ####
params_oak <- LWFBrook90R::set_paramLWFB90(zw = 10,
                                           obsheight = 0.02,
                                           dswmax = 0.5,
                                           dpsimax = 0.01,
                                           winlaifrac = 0,

                                           budburst_species = "Quercus robur",

                                           alb = 0.21,
                                           albsn = 0.47,
                                           lwidth = 0.05,
                                           rhotp = 2,
                                           #glmax = 0.0053,
                                           glmax = 0.007,
                                           radex = 0.59,
                                           glmin = 0.0003,
                                           mxkpl = 8,
                                           maxrlen = 3200,
                                           psicr = -2.5,
                                           rrad = 0.25,

                                           cintrl = 0.7,
                                           cintrs = 1,
                                           cintsl = 2.8,
                                           cintss = 4,
                                           frintlai = 0.1,
                                           frintsai = 0.1,
                                           fsintlai = 0.1,
                                           fsintsai = 0.5,

                                           maxlai = 4.5,
                                           sai = 0.9,
                                           height = 26,
                                           age_ini = 100,
                                           maxrootdepth = -2,
                                           betaroot = 0.966)


# spruce ------- ####
params_spruce <- LWFBrook90R::set_paramLWFB90(zw = 10,
                                              obsheight = 0.02,
                                              dswmax = 0.5,
                                              dpsimax = 0.01,

                                              budburst_species = "Picea abies (spaet)",

                                              alb = 0.13,
                                              albsn = 0.34,
                                              lwidth = 0.001,
                                              rhotp = 2.6,
                                              #glmax = 0.00414,
                                              glmax = 0.0035,
                                              radex = 0.45,
                                              glmin = 0.0001,
                                              mxkpl = 8,
                                              maxrlen = 3000,
                                              psicr = -2,
                                              rrad = 0.25,

                                              cintrl = 0.4,
                                              cintrs = 0.2,
                                              cintsl = 1.6,
                                              cintss = 0.8,
                                              frintlai = 0.08,
                                              frintsai = 0.08,
                                              fsintlai = 0.08,
                                              fsintsai = 0.1,

                                              maxlai = 7,
                                              sai = 2,
                                              winlaifrac = 0.8,
                                              height = 36,
                                              age_ini = 100,
                                              maxrootdepth = -1.2,
                                              betaroot = 0.976)


# pine --------- ####
params_pine <- LWFBrook90R::set_paramLWFB90(zw = 10,
                                            obsheight = 0.02,
                                            dswmax = 0.5,
                                            dpsimax = 0.01,

                                            budburst_species = "Pinus sylvestris",

                                            alb = 0.13,
                                            albsn = 0.34,
                                            lwidth = 0.001,
                                            rhotp = 2.6,
                                            #glmax = 0.0053,
                                            glmax = 0.005,
                                            radex = 0.45,
                                            glmin = 0.0002,
                                            mxkpl = 8,
                                            maxrlen = 3000,
                                            psicr = -2.5,
                                            rrad = 0.25,

                                            cintrl = 1,
                                            cintrs = 1,
                                            cintsl = 4,
                                            cintss = 4,
                                            frintlai = 0.13,
                                            frintsai = 0.13,
                                            fsintlai = 0.13,
                                            fsintsai = 0.3,

                                            maxlai = 3.5,
                                            sai = 0.8,
                                            winlaifrac = 0.5,
                                            height = 30,
                                            age_ini = 100,
                                            maxrootdepth = -2,
                                            betaroot = 0.976)


# douglasfir --- ####
params_douglasfir <- LWFBrook90R::set_paramLWFB90(zw = 10,
                                                  obsheight = 0.02,
                                                  dswmax = 0.5,
                                                  dpsimax = 0.01,

                                                  budburst_species = "Picea abies (spaet)",

                                                  alb = 0.13,
                                                  albsn = 0.34,
                                                  lwidth = 0.001,
                                                  rhotp = 2.6,
                                                  #glmax = 0.00414,
                                                  glmax = 0.0025,
                                                  radex = 0.45,
                                                  glmin = 0.0001,
                                                  mxkpl = 8,
                                                  maxrlen = 3000,
                                                  psicr = -2.5,
                                                  rrad = 0.25,

                                                  cintrl = 0.4,
                                                  cintrs = 0.2,
                                                  cintsl = 1.6,
                                                  cintss = 0.8,
                                                  frintlai = 0.12,
                                                  frintsai = 0.2,
                                                  fsintlai = 0.08,
                                                  fsintsai = 0.1,

                                                  maxlai = 6,
                                                  sai = 0.8,
                                                  winlaifrac = 0.8,
                                                  height = 40,
                                                  age_ini = 100,
                                                  maxrootdepth = -1.6,
                                                  betaroot = 0.976)


# MRS-beech ---- ####
params_MRS_beech <- LWFBrook90R::set_paramLWFB90(winlaifrac = 0.1,
                                                 maxlai = 6,
                                                 sai = 1,
                                                 height = 30,
                                                 frintlai = 0.12,
                                                 frintsai = 0.2,
                                                 cintrl = 0.1,
                                                 cintrs = 0.6,
                                                 fsintlai = 0.2,
                                                 fsintsai = 0.6,
                                                 cintss = 0.6,
                                                 cintsl = 0.6,
                                                 alb = 0.18,
                                                 albsn = 0.23,
                                                 lwidth = 0.05,
                                                 glmax = 0.0042,
                                                 infexp = 0.66,

                                                 budburst_species = "Fagus sylvatica")
# MRS-spruce --- ####
params_MRS_spruce <- LWFBrook90R::set_paramLWFB90(winlaifrac = 0.8,
                                                  maxlai = 5.5,
                                                  sai = 1,
                                                  height = 30,
                                                  frintlai = 0.12, #0.1
                                                  frintsai = 0.14, #0.15
                                                  cintrl = 0.2, #0.1
                                                  cintrs = 0.4, #0.8
                                                  fsintlai = 0.12,
                                                  fsintsai = 0.14,
                                                  cintss = 0.6,
                                                  cintsl = 0.6,
                                                  alb = 0.14,
                                                  albsn = 0.14,
                                                  lwidth = 0.004,
                                                  glmax = 0.0035,
                                                  infexp = 0.66,

                                                  budburst_species = "Picea abies (frueh)")

# save ####
params_default <- LWFBrook90R::set_paramLWFB90()

save(params_beech, params_oak, params_pine, params_spruce, params_douglasfir, params_default,
     params_MRS_beech, params_MRS_spruce,
     file = "./data/params_species.rda")
# load("J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90/data/params_species.rda")

# df.output
df.output <- LWFBrook90R::set_outputLWFB90()
df.output[,] <- 0L
df.output[c("Budg","Evap", "Flow", "Swat"), c("Day")] <- 1
df.output[c("Flow"), c("Mon")] <- 1

save(df.output,
     file = "./data/df_output.rda")

# paths ####

# ... to wuchsgebiet-Shapefile:
path_WGB_diss_shp = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/WUGEB/"

# ...to STOK in 7 wuchsgebiets-splits:
path_STOK_pieces = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/STOK/"

# ...to GEOLA in 7 wuchsgebiets-splits:
path_GEOLA_pieces = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/GEOLA/"

# ...to altitude, slope and aspect:
path_DGM = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/DGM/"

# ...to altitude, slope and aspect:
path_DGM_D = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/DGM_D/"


# ...to original BZE-reg data:
path_models_in = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/BZE_REG_filled/original/"

# ...to filled BZE_reg data:
path_BZEreg = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/BZE_REG_filled/filled/"

# ...to PDUR-layer:
path_pdur = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/PDUR/"
path_pdur_D = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/PDUR_D/"

# ...to PDUR-layer:
path_clim = "R:/klima/whh/brook90_input/rds/"

# ...to BSK-forest-area (continuously updated):
path_bsk_forest = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/WALD_LY/Waldbesitz_und_waldmaske_filter_10000_m2.shp"

# ... to LeitprofilDB
path_df.LEIT = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/LEIT_DB/LP_DB.rds"

# ... to UHH_empty
path_UHH = "J:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90_data/UHH/"


save(path_WGB_diss_shp,
     path_STOK_pieces, path_GEOLA_pieces, path_DGM, path_DGM_D,
     path_models_in, path_BZEreg,
     path_pdur, path_bsk_forest,
     path_df.LEIT,
     path_clim,
     path_UHH,
     file = "./data/paths.rda")

