#' Quality metrics
#'
#' @param data Input data
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
#' @examples 
#' quality(sampdat)
quality <- function(data){
  data <- data[which(data$AnalyteName %in% c("Alkalinity as CaCO3", "Oxygen, Dissolved",
                                                      "pH", "Salinity", "SpecificConductivity",
                                                      "Temperature", "Turbidity")),]
  XWAK <- tapply(data$Result[data$AnalyteName=="Alkalinity as CaCO3"], 
                 data$id[data$AnalyteName=="Alkalinity as CaCO3"], mean)
  XWDO <- tapply(data$Result[data$AnalyteName=="Oxygen, Dissolved"], 
                 data$id[data$AnalyteName=="Oxygen, Dissolved"], mean)
  XWPH <- tapply(data$Result[data$AnalyteName=="pH"], 
                 data$id[data$AnalyteName=="pH"], mean)
  XWSL <-  tapply(data$Result[data$AnalyteName=="Salinity"], 
                  data$id[data$AnalyteName=="Salinity"], mean)
  XWSC <- tapply(data$Result[data$AnalyteName=="SpecificConductivity"], 
                 data$id[data$AnalyteName=="SpecificConductivity"], mean)
  XWTC <- tapply(data$Result[data$AnalyteName=="Temperature"], 
                 data$id[data$AnalyteName=="Temperature"], mean)
  XWTF <- tapply(data$Result[data$AnalyteName=="Temperature"], 
                 data$id[data$AnalyteName=="Temperature"], function(d)(mean(d)*(9/5))+32)
  XWTB <- tapply(data$Result[data$AnalyteName=="Turbidity"], 
                 data$id[data$AnalyteName=="Turbidity"], mean)
  
  quality_metrics <- as.data.frame(matrix(NA, nrow=length(unique(data$id)), ncol=8))
  rownames(quality_metrics)<- unique(data$id)
  quality_metrics[which(rownames(quality_metrics)%in%names(XWAK)), 1]<-XWAK
  quality_metrics[which(rownames(quality_metrics)%in%names(XWDO)), 2]<-XWDO
  quality_metrics[which(rownames(quality_metrics)%in%names(XWPH)), 3]<-XWPH
  quality_metrics[which(rownames(quality_metrics)%in%names(XWSL)), 4]<-XWSL
  quality_metrics[which(rownames(quality_metrics)%in%names(XWSC)), 5]<-XWSC
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTC)), 6]<-XWTC
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTF)), 7]<-XWTF
  quality_metrics[which(rownames(quality_metrics)%in%names(XWTB)), 8]<-XWTB
  colnames(quality_metrics)<- c("XWAK.result", "XWDO.result", "XWPH.result", "XWSL.result", "XWSC.result",
                                "XWTC.result", "XWTF.result", "XWTB.result")
  
  return(quality_metrics)
  
}