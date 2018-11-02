#' Disturbance metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @examples 
#' disturbance(sampdat)
disturbance <- function(data){
  data <- data[which(data$AnalyteName %in% c('Riparian Bridges/Abutments', 'Riparian Buildings', 'Riparian Landfill/Trash', 'Riparian Logging', 'Riparian Mining', 'Riparian Orchards/Vineyards', 'Riparian Park/Lawn', 'Riparian Pasture/Range', 'Riparian Pavement', 'Riparian Pipes', 'Riparian Road', 'Riparian Row Crops', 'Riparian Vegetation Management', 'Riparian Wall/Dike')),]
  ###Set up###

  data2 <- gsub("(Block|Channel)(\\w)", "\\2", data$LocationCode)
  data2 <- gsub(",\\sLeft", "", data2)
  data2 <- gsub(",\\sRight", "", data2)
  
  reformed <- data.frame(data$id, gsub("(Block|Channel)(\\w)", "\\1", data$LocationCode), data2,
                         data$AnalyteName, data$VariableResult)
  colnames(reformed) <- c("id", "Location", "Trans", "AnalyteName", "VariableResult")
  reformed$VariableResult <- as.character(reformed$VariableResult)
  reformed$VariableResult[reformed$VariableResult=="B"] <- "1.5"
  reformed$VariableResult[reformed$VariableResult=="P"] <- "0.667"
  reformed$VariableResult[reformed$VariableResult=="C"] <- "1"
  reformed$VariableResult[reformed$VariableResult=="N"] <- "100"
  reformed$VariableResult[reformed$VariableResult=="Y"] <- "200"
  reformed$VariableResult <- as.numeric(reformed$VariableResult)

  casting <- function(reformed){
    casted <- as.data.frame(reshape::cast(reformed, 
                                 id+Trans ~ Location, value = "VariableResult", fun.aggregate='length'))
    if(is.null(casted$channel)==T){casted[[5]] <- rep(100, times=length(casted$id))}
    colnames(casted) <- c("id", "Trans", "Left", "Right", "Channel")
    casted$sums <- rep(NA, length(casted$Channel))
    for(i in 1:length(casted$Channel)){
      a <- casted$Left[i]
      b <- casted$Right[i]
      if(casted$Channel[i]==200){casted$sums[i]=3} else{
        if(casted$Channel[i]==100){casted$sums[i] <- (a+b)}
      }
    }
    return(casted)
  }
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  ###Create the data frame###
  analytes <- unique(as.character(reformed$AnalyteName))
  statname <- c("W1H_BRDG", "W1H_BLDG", "W1H_LDFL", "W1H_LOG", "W1H_MINE", "W1H_ORVY", "W1H_PARK",
                "W1H_PSTR", "W1H_PVMT", "W1H_PIPE", "W1H_ROAD", "W1H_CROP", "W1H_VEGM", "W1H_WALL")
  
  reformed$AnalyteName <- as.character(reformed$AnalyteName)
  
  result <- matrix(NA, nrow=length(unique(reformed$id)), ncol=(length(statname)*3))
  result <- as.data.frame(result)
  rownames(result) <- unique(reformed$id)
  colnames(result) <- paste(rep(statname, each=3), c(".result", ".count", ".sd"), sep="")
  
  
  ###Compute Stats###
  for(i in 1:length(analytes)){
    analyte <- casting(subset(reformed, reformed$AnalyteName == analytes[i]))
    analytesum <- tapply(analyte$sum, analyte$id, sumna)
    analytetotal <- tapply(analyte$sum, analyte$id, lengthna)
    analytemean <- analytesum/(2*analytetotal)
    analytesd <- tapply(analyte$sum, analyte$id, sd)
    result[[((i-1)*3)+1]] <- analytemean
    result[[((i-1)*3)+2]] <- analytetotal
    result[[((i-1)*3)+3]] <- analytesd
  }
  
  W1_HALL_EMAP.result <- result$W1H_BLDG.result + result$W1H_LDFL.result + result$W1H_LOG.result + result$ W1H_MINE.result 
  + result$W1H_PARK.result + result$W1H_PSTR.result + result$W1H_PVMT.result + result$W1H_PIPE.result + 
    result$W1H_ROAD.result + result$W1H_CROP.result + result$W1H_WALL.result
  
  W1_HALL_SWAMP.result <- result$W1H_BLDG.result + result$W1H_LDFL.result + result$W1H_LOG.result + result$ W1H_MINE.result 
  + result$W1H_PARK.result + result$W1H_PSTR.result + result$W1H_PVMT.result + result$W1H_PIPE.result + 
    result$W1H_ROAD.result + result$W1H_CROP.result + result$W1H_WALL.result + result$ W1H_BRDG.result +
    result$W1H_ORVY.result + result$W1H_VEGM.result
  
  results <- cbind(result, W1_HALL_EMAP.result, W1_HALL_SWAMP.result)

  return(results)
  
}