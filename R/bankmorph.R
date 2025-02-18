#' Bank morphology metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples 
#' bankmorph(sampdat)
bankmorph <- function(data){

  data <- data[which(data$AnalyteName %in% c('Bankfull Height', 'Bankfull Width', 'StationWaterDepth', 'Wetted Width')),]
  data <- data[which(!(data$LocationCode == "X")),]
  data$Result[data$Result == -88] <- NA
  ###Slice Bankfull Height Data ###
  
  heightindex <- which(data$AnalyteName == "Bankfull Height")
  bankfullheight <- data.frame(cbind(data$id[heightindex], as.character(data$AnalyteName[heightindex]),
                                     as.character(data$Result[heightindex])))
  colnames(bankfullheight) <- c("id", "AnalyteName", "result")
  bankfullheight$result <- as.numeric(as.character(bankfullheight$result))
  head(bankfullheight)
  
  ###Compute XBKF_H###
  sumna <- function(data){
    sum(as.numeric(as.character(data)), na.rm = T)
  }
  XBKF_sum <- tapply(bankfullheight$result, bankfullheight$id, sumna)
  lengthna <- function(data){
    sum(!is.na(data))
  }
  XBKF_H.count <- tapply(bankfullheight$result, bankfullheight$id, lengthna)
  XBKF_H.result <- XBKF_sum/XBKF_H.count
  sdna <- function(data){
    sd(data, na.rm = T)
  }
  XBKF_H.sd <- tapply(as.numeric(bankfullheight$result), bankfullheight$id, sdna)
  XBKF_H <- data.frame(cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd))
  
  ###Slice Bankfull Width###
  
  widthindex <- which(data$AnalyteName == "Bankfull Width")
  bankfullwidth <- data.frame(cbind(data$id[widthindex], as.character(data$AnalyteName[widthindex]),
                                    as.character(data$Result[widthindex])))
  colnames(bankfullwidth) <- c("id", "AnalyteName", "result")
  bankfullwidth$result <- as.numeric(as.character(bankfullwidth$result))

  ###Compute XBKF_W###
  
  XBKF_W_sum <- tapply(bankfullwidth$result, bankfullwidth$id, sumna)
  XBKF_W.count <- tapply(bankfullwidth$result, bankfullwidth$id, lengthna)
  XBKF_W.result <- XBKF_W_sum/XBKF_W.count
  XBKF_W.sd <- tapply(as.numeric(bankfullwidth$result), bankfullwidth$id, sdna)
  XBKF_W <- data.frame(cbind(XBKF_W.result, XBKF_W.count, XBKF_W.sd))
  
  ###XWDEPTH###
  
  XWDEPTHdata <- data.frame(cbind(data$id[which(data$AnalyteName == "StationWaterDepth")], 
                                  as.numeric(as.character(data$Result[which(data$AnalyteName == "StationWaterDepth")]))))
  colnames(XWDEPTHdata) <- c("id", "result")
  XWDEPTH_sum <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, sumna)
  XWDEPTH.count <- tapply(XWDEPTHdata$result, XWDEPTHdata$id, lengthna)
  XWDEPTH.result <- XWDEPTH_sum/XWDEPTH.count
  XWDEPTH.sd <- tapply(as.numeric(XWDEPTHdata$result), XWDEPTHdata$id, sdna)
  
  ###XWIDTH###
  
  XWIDTHdata <- data.frame(cbind(data$id[which(data$AnalyteName == "Wetted Width")], 
                                 as.numeric(as.character(data$Result[which(data$AnalyteName == "Wetted Width")]))))
  colnames(XWIDTHdata) <- c("id", "result")
  XWIDTH_sum <- tapply(XWIDTHdata$result, XWIDTHdata$id, sumna)
  XWIDTH.count <- tapply(XWIDTHdata$result, XWIDTHdata$id, lengthna)
  XWIDTH.result <- XWIDTH_sum/XWIDTH.count
  XWIDTH.sd <- tapply(as.numeric(XWIDTHdata$result), XWIDTHdata$id, sdna)
  
  ###XWDR###
  
  XWDR.result <- (XWIDTH.result/XWDEPTH.result)*100
  XWDR.count<- XWIDTH.count
  ###XWDA###
  
  XWDA.result <- XWDEPTH.result/(XWIDTH.result*10)
  XWDA.count <- XWDEPTH.count
  ###XWDM###

  data$LocationCode2 <- gsub("Tran.(\\w)", "\\1", data$LocationCode)

  ll <- c( ", LCtr" , ", Ctr" , ", RCtr",
           ", Right", ", Left")
  for(i in 1:length(ll)){
    data$LocationCode2 <-gsub(ll[i], "", data$LocationCode2)}
  XWDM_max <- data %>% 
    dplyr::filter(AnalyteName %in% 'StationWaterDepth') %>% 
    dplyr::group_by(id, LocationCode2) %>% 
    dplyr::summarize(Result = max(Result)) %>% 
    tidyr::spread(LocationCode2, Result) 

  XWDM_max<- XWDM_max[,which(!(1:length(colnames(XWDM_max)) %in% grep("Float", colnames(XWDM_max))))]
  dim <- XWDM_max[[1]]
  XWDM_max<-XWDM_max[, which(!(1:(length(XWDM_max)-1) %in% grep("Section", (colnames(XWDM_max)))))]
  XWDM_max <- XWDM_max[, 2:length(XWDM_max)]
  XWDM.result <- rowSums(XWDM_max, na.rm = T)/apply(XWDM_max, 1, lengthna)
  names(XWDM.result) <- dim
  XWDM.count <-apply(XWDM_max, 1, lengthna)
  
  ###Write to file###
  results <- cbind(XBKF_H.result, XBKF_H.count, XBKF_H.sd, XBKF_W.result, XBKF_W.count, XBKF_W.sd, XWDEPTH.result, 
                   XWDEPTH.count, XWDEPTH.sd, XWIDTH.result, XWIDTH.count, XWIDTH.sd, XWDR.result, XWDR.count, 
                   XWDA.result, XWDA.count, XWDM.result , XWDM.count)

  return(results)
}