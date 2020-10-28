#' Short filter function to filter the in-between-ness of df-rows regarding vectors
#'
#' Filtering multiple rows according to multiple target depths
#'
#'
#' @param df data frame with \code{upper} and \code{lower}
#' @param depths target depth
#'
#' @return filtered dataframe
#'

fnc_filter <- function(df, depths){
  for(i in depths){
    df.tmp <- df %>%
      dplyr::filter(upper >= i & lower < i) %>%
      dplyr::mutate(depth = i)
    if(i == depths[1]){
      df.final <- df.tmp
    }else{
      df.final <- rbind(df.final, df.tmp)
    }

  }
  return(df.final)
}
