#' Save error messages and simulation time
#'
#' When running multiple points with \code{\link[LWFBrook90]{run_multisite_LWFB90}} and storing the results in an sqlite-database with \code{\link{fnc_write_to_sql}}, you lose the information on how long each simulation took (which is an indicator for flawed input data), and which ones failed and why. This function helps storing this information.
#'
#' @param res results of a \code{run_multisite_LWFB90} execution, saved by an assignment operator, see example
#' @param meta a data frame with the following columns
#' \itemize{
#' \item \code{ID_custom} - unique matching the IDs of df.ids
#' \item \code{modelled} - status whether modelled or not. All \code{0} in the beginning. Will be changed to \code{1} if successful
#' \item \code{err.code} - potential error codes will be stored here. Recommended to be set to \code{no error}
#' \item \code{sim_dur_s} - an empty column of /code{NA_integer_}
#' }
#' Will be created if not provided.
#'
#' @return returns a the meta-dataframe with error messages and simulation times added
#'
#' @example inst/examples/fnc_check_errors_ex.R
#'
#' @import stringr
#' @export

fnc_check_errors <- function(res,
                             meta = NA){

  if(is.na(meta)){
    ids_modelled <- unlist(lapply(stringr::str_split(names(res), " "), `[[`, 1))

    meta <- data.frame("ID_custom" = ids_modelled,
                       modelled = 0,
                       err.code = "no error",
                       sim_dur_s = NA_integer_)

  }

  # which worked, which didn't
  suc.mod <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(lapply(res, function(x) names(x)), "simulation_duration")],
                                     " "), `[[`, 1))
  err.mod <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(lapply(res, function(x) names(x)), "message")],
                                     " "), `[[`, 1))

  sim.dur.df <- data.frame("ID_custom" = unlist(lapply(stringr::str_split(names(res), " "), `[[`, 1)),

                           "sim_dur_s" = as.numeric(lapply(res, function(x) round(as.numeric(x$simulation_duration), 2))))

  # retrieve potential errorcodes
  err.mod.1 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "initial matrix")], " "), `[[`, 1))
  err.mod.2 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "FWETK failed")], " "), `[[`, 1))
  err.mod.3 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "inconsistent dates in climate")], " "), `[[`, 1))
  err.mod.4 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "inconsistent dates in precipitation")], " "), `[[`, 1))
  err.mod.5 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "wrong precipitation")], " "), `[[`, 1))
  err.mod.6 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "negative soil water storage")], " "), `[[`, 1))
  err.mod.7 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "water storage exceeds water capacity")], " "), `[[`, 1))
  err.mod.8 <- unlist(lapply(stringr::str_split(names(res)[stringr::str_detect(res, "Zeitlimit erreicht")], " "), `[[`, 1))

  # change meta
  meta <- meta %>%
    mutate(modelled = case_when(ID_custom %in% suc.mod ~ 1,
                                ID_custom %in% err.mod ~ 0,
                                T ~ modelled),
           err.code = case_when(ID_custom %in% err.mod.1 ~ "Simulation terminated abnormally: 'initial matrix psi > 0'",
                                ID_custom %in% err.mod.2 ~ "Simulation initialization failed: 'FWETK failed to determine wetness at KF'",
                                ID_custom %in% err.mod.3 ~ "Simulation terminated abnormally: 'inconsistent dates in climate!'",
                                ID_custom %in% err.mod.4 ~ "Simulation terminated abnormally: 'inconsistent dates in precipitation input!'",
                                ID_custom %in% err.mod.5 ~ "Simulation terminated abnormally: 'wrong precipitation interval input!'",
                                ID_custom %in% err.mod.6 ~ "Simulation terminated abnormally: 'negative soil water storage!'",
                                ID_custom %in% err.mod.7 ~ "Simulation terminated abnormally: 'water storage exceeds water capacity!'",
                                ID_custom %in% err.mod.8 ~ "Simulation terminated: 'time limit reached'",
                                T ~ err.code))

  meta[which(!is.na(match(meta$ID_custom, sim.dur.df$ID_custom))), "sim_dur_s"] <- sim.dur.df$sim_dur_s

  return(meta)
}
