#' Calculate all PHAB metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' phabmetrics(sampdat)
#' }
phabmetrics <- function(data){
  data <- phabformat(data)
  data <- chkinp(data, purge = TRUE)
  metrics <- list(bankmorph(data), channelmorph(data), channelsinuosity(data),
                  densiometer(data),  habitat(data), disturbance(data), flow(data),
                  misc(data), bankstability(data), quality(data), ripveg(data),
                  substrate(data), algae(data))
  cbind.fill<-function(...){
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(NA, n-nrow(x), ncol(x))))) 
  }

  # concatenate, NaN to NA
  data <- do.call(cbind.fill, metrics)
  data <- gsub('NaN', NA, data)

  # Changing final output to be a dataframe
  # do not convert columns to numeric if contain characters
  stnm <- row.names(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE) %>% 
    dplyr::mutate_if(
      ~ !any(grepl('[a-z,A-Z]', .x)), as.numeric
      )
  rownames(data) <- stnm
  
  return(data)
}
