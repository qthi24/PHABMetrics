#' Format input data
#'
#' @param data Input data
#'
#' @export
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' phabformat(sampdat)
phabformat <- function(data){
  data = data.frame(data)
  data$VariableResult[data$ResQualCode=="NR"] <- "Not Recorded"
  data$Result[data$ResQualCode=="NR"] <- NA
  data <- data %>% 
    tidyr::unite('id', StationCode, SampleDate, Replicate, remove = F) %>% 
    data.frame(stringsAsFactors = F)
  return(data.frame(data))
}

