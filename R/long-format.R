
#' Format output in long form
#'
#' @param out 
#' 
#' @importFrom magrittr "%>%" 
#' @importFrom tidyr gather separate 
#' @importFrom dplyr select 
#' @importedFrom tibble rownames_to_column
#'
#' @return result data frame in long format
#' @export
#'
#' @examples
#' \dontrun{
#' res <- long_form(out)
#' }
long_form <- function(out){
  
  # result ------------------------------------------------------------------
  
  res <- out %>%
    rownames_to_column('StationCode') %>% 
    select(StationCode, contains('result'))
  colnames(res) <- sub('.result', '', colnames(res))
  res_gather <- res %>%
    gather('name', 'result', -StationCode)
  
  
  # standard deviation ------------------------------------------------------
  
  stdv <- out %>%
    select(StationCode, contains('sd'))
  colnames(stdv) <- sub('.sd', '', colnames(stdv))
  stdv_gather <- stdv %>%
    gather('name', 'sd', -StationCode)
  
  
  # count -------------------------------------------------------------------
  
  counts <- out %>%
    select(StationCode, contains('count'))
  colnames(counts) <- sub('.count', '', colnames(counts))
  counts_gather <- counts %>%
    gather('name', 'count', -StationCode)
  
  
  # join --------------------------------------------------------------------
  
  test <- full_join(
    res_gather, stdv_gather, by = c('name', 'StationCode')
  )
  
  all3 <- full_join(
    test, counts_gather, by = c('name', 'StationCode') 
    ) %>% 
    separate(StationCode, c('StationCode', 'SampleDate'), sep = '_')
  
  return(all3)
    
}




