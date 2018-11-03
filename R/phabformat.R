#' Format input data
#'
#' @param data Input data
#'
#' @export
#' 
#' @examples
#' phabformat(sampdat)
phabformat <- function(data){
  data$VariableResult[data$ResQualCode=="NR"] <- "Not Recorded"
  data$Result[data$ResQualCode=="NR"] <- NA
  data$id <- do.call(paste, c(data[c("StationCode", "SampleDate")]))
  return(data)
}
