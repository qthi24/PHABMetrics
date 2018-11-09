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

  data <- do.call(cbind.fill, metrics)
  return(data)
}
