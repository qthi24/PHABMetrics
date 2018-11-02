#' Habitat metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @examples 
#' habitat(sampdat)
habitat <- function (data) {
  data <- data[which(data$AnalyteName %in% c('Fish Cover Macrophytes', 'Fish Cover Artificial Structures', 'Fish Cover Boulders', 'Fish Cover Filamentous Algae', 'Fish Cover Woody Debris >0.3 m', 'Fish Cover Live Trees/Roots', 'Fish Cover Overhang.Veg', 'Fish Cover Woody Debris <0.3 m', 'Fish Cover Undercut Banks')),]
  data$id <- do.call(paste, c(data[c("StationCode", "SampleDate")]))
  
  data$convert <- rep(NA, length(data$StationCode))
  
  data$convert<-gsub("1", "5",data$VariableResult)
  data$convert<-gsub("2", "25",data$convert)
  data$convert<-gsub("3", "57.5",data$convert)
  data$convert<-gsub("4", "87.5",data$convert)
  
  ###Compute Stats
  analytes <- c("Fish Cover Macrophytes", "Fish Cover Artificial Structures", "Fish Cover Boulders",
                "Fish Cover Filamentous Algae", "Fish Cover Woody Debris >0.3 m", 
                "Fish Cover Live Trees/Roots", "Fish Cover Overhang.Veg", "Fish Cover Woody Debris <0.3 m",
                "Fish Cover Undercut Banks")
  statname <- c("XFC_AQM",  "XFC_HUM",  "XFC_RCK",  "XFC_ALG",	"XFC_LWD",
                "XFC_LTR",	"XFC_OHV",	"XFC_BRS",	"XFC_UCB",	
                "XFC_BIG.result",	"XFC_NAT_EMAP.result",	"XFC_NAT_SWAMP.result",	
                "CFC_AQM.result",	"CFC_HUM.result",	"CFC_RCK.result",	"CFC_ALG.result",	"CFC_LWD.result",
                "CFC_LTR.result",	"CFC_OHV.result",	"CFC_BRS.result",	"CFC_UCB.result",	
                "CFC_ALL_EMAP.result",	"CFC_ALL_SWAMP.result")
  
  result <- matrix(NA, nrow=length(unique(data$id)), 
                   ncol=((length(statname[1:9])*3)+length(statname[10:23])))
  result <- as.data.frame(result)
  rownames(result) <- unique(data$id)
  snames <- paste(rep(statname[1:9], each=3), c(".result", ".count", ".sd"), sep="")
  colnames(result) <- c(snames, statname[10:23])
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  data$convert <- as.numeric(data$convert)
  
  for(i in 1:9){
    analyte <- subset(data, data$AnalyteName == analytes[i])
    analytesum <- tapply(analyte$convert, analyte$id, sumna)
    analytetotal <- tapply(analyte$convert, analyte$id, lengthna)
    analytemean <- analytesum/analytetotal
    analytesd <- tapply(analyte$convert, analyte$id, sd)
    result[[((i-1)*3)+1]] <- analytemean
    result[[((i-1)*3)+2]] <- analytetotal
    result[[((i-1)*3)+3]] <- analytesd
  }
  
  result$XFC_BIG.result <-  result$XFC_LWD.result + result$XFC_RCK.result + 
    result$XFC_UCB.result + result$XFC_HUM.result
  
  result$XFC_NAT_EMAP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
    result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result
  
  result$XFC_NAT_SWAMP.result <- result$XFC_LWD.result + result$XFC_BRS.result +
    result$XFC_OHV.result + result$XFC_RCK.result + result$XFC_UCB.result +
    result$XFC_LTR.result + result$XFC_AQM.result
  
  data$present <- as.numeric(as.character(data$VariableResult)) >= 1
  
  for(i in 1:9){
    analyte <- subset(data, data$AnalyteName == analytes[i])
    analytesum <- tapply(analyte$present, analyte$id, sumna)
    result[[i+30]] <- analytesum
  }
  
  t <- result[,c("XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
  result$CFC_ALL_EMAP.result <- rowSums((t>0), na.rm=T)
  
  q <- result[,c("XFC_LTR.result", "XFC_AQM.result", "XFC_HUM.result", "XFC_RCK.result", "XFC_ALG.result", "XFC_LWD.result", "XFC_OHV.result", "XFC_BRS.result", "XFC_UCB.result")]
  result$CFC_ALL_SWAMP.result <- rowSums((q>0), na.rm=T)

  return(result)
  
}