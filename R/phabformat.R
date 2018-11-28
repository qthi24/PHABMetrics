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
  data <- transform(data, StationCode = as.character(StationCode))
  data <- transform(data, SampleDate = as.POSIXct(SampleDate))
  data <- transform(data, Replicate = as.integer(Replicate))
  data <- transform(data, LocationCode = as.character(LocationCode))
  data <- transform(data, AnalyteName = as.character(AnalyteName))
  data <- transform(data, UnitName = as.character(UnitName))
  data <- transform(data, VariableResult = as.character(VariableResult))
  data <- transform(data, FractionName = as.character(FractionName))
  data <- transform(data, Result = as.numeric(Result))
  data <- transform(data, ResQualCode = as.character(ResQualCode))
  data <- transform(data, QACode = as.character(QACode))
  data <- transform(data, id = as.character(id))
  #data <- tibble::as_tibble(data)
  data$VariableResult[data$ResQualCode=="NR"] <- "Not Recorded"
  data$Result[data$ResQualCode=="NR"] <- NA
  data <- data %>% 
    tidyr::unite('id', StationCode, SampleDate, Replicate, remove = F) %>% 
    data.frame(stringsAsFactors = F)
  return(data)
}

