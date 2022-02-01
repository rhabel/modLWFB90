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
                                                  glmax = 0.0035,
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


params_default <- LWFBrook90R::set_paramLWFB90()


save(params_beech, params_oak, params_pine, params_spruce, params_douglasfir, params_default,
     file = "./data/params_species.rda")
# load("H:/FVA-Projekte/P01540_WHHKW/Programme/Eigenentwicklung/modLWFB90/data/params_species.rda")
# save(input_bze, input_clim, input_gis, input_paul, input_stok,
#      file = "./data/paths.rda")
