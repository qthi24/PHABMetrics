#' Flow metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @examples 
#' flow(sampdat)
flow <- function(data){

  data <- data[which(data$AnalyteName %in% c('Distance from Bank', 'StationWaterDepth', 'Velocity', 'Distance, Float', 'Float Time', 'Wetted Width')),]
  velocity<-data
  flow<-data
  neutral<-data
  
  test <-velocity[, c("StationCode", "SampleDate", "Replicate", "AnalyteName", "Result",  "ResQualCode", "QACode")]
  test2 <- flow[, c("StationCode", "SampleDate", "Replicate", "AnalyteName", "Result",  "ResQualCode", "QACode")]
  
  tempp <- (rbind(test, test2))
  tempp$id <- do.call(paste, c(tempp[c("StationCode", "SampleDate")]))
  vmethod <- (reshape::cast(tempp, id + Replicate ~ AnalyteName, value="Result", fun.aggregate=mean))
  
  
  ###Calculate Velocity Area Method###
  vmethod$flow <- rep(NA, length(vmethod$id))
  
  vmethod$flow <-c(NA, unlist(lapply(2:length(vmethod$id), FUN=function(i, d, v, s){((d[i]-d[i-1]))*s[i]*v[i]*0.00107639104},
                                     d=vmethod$"Distance from Bank", s=vmethod$StationWaterDepth, v=vmethod$Velocity)))
  sumna <- function(data){sum(as.numeric(as.character(data)), na.rm = T)}
  FL_Q_F<-tapply(vmethod$flow, vmethod$id, sumna)
  FL_Q_F[which(FL_Q_F==0)] <-NA
  FL_Q_F[which(FL_Q_F<0)] <-0
  FL_Q_M <- FL_Q_F*0.0283168466
  
  ###Format Data Frame###
  neutral$Result[neutral$ResQualCode=="NR"] <- NA
  neutral$id<- do.call(paste, c(neutral[c("StationCode", "SampleDate")]))
  neutral$Location <- rep(NA, length(neutral$id))
  neutral$Location[grep("Upper Section Point", neutral$LocationCode)] <- "Upper"
  neutral$Location[grep("Middle Section Point", neutral$LocationCode)] <- "Middle"
  neutral$Location[grep("Lower Section Point", neutral$LocationCode)] <- "Lower"
  neutral$Location[grep("Float Reach TransUpper", neutral$LocationCode)] <- "Upper"
  neutral$Location[grep("Float Reach TransMiddle, Point", neutral$LocationCode)] <- "Middle"
  neutral$Location[grep("Float Reach TransLower", neutral$LocationCode)] <- "Lower"
  neutral$Location[intersect(grep("Lower Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "L"
  neutral$Location[intersect(grep("Middle Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "M"
  neutral$Location[intersect(grep("Upper Section", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "U"
  neutral$Location[intersect(grep("Float Reach TransLower", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "L"
  neutral$Location[intersect(grep("Float Reach TransMiddle", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "M"
  neutral$Location[intersect(grep("Float Reach TransUpper", neutral$LocationCode), (which(neutral$AnalyteName=="Wetted Width")))] <- "U"
  
  ###Calculate neutral buoyant method###

  # do not calculate if not provided
  if(all(is.na(neutral$Location))){
    
    uniid <- sort(unique(neutral$id))
    FL_N_M <- rep(NaN, length(uniid))
    names(FL_N_M) <- uniid
    FL_N_F <- rep(NaN, length(uniid))
    names(FL_N_F) <- uniid
    
  # otherwise calculate    
  } else {
    
    ncast <- reshape::cast(neutral[!neutral$LocationCode %in% "Float Reach",], id~Location, value="Result", fun.aggregate=mean, na.rm = T)
    ncast$r <- rep(NA, length(ncast$id))
    ncast$r <- lapply(1:length(ncast$id), function(i, l, lower, m, middle, u, upper){
      mean((l[i]*lower[i]),(m[i]*middle[i]),(u[i]*upper[i]), na.rm=T, trim=0)/100},
      l=ncast$L, lower=ncast$Lower, m=ncast$M, middle=ncast$Middle, u=ncast$U, upper=ncast$Upper)
    narea <- tapply(ncast$r, ncast$id, invisible)
    
    ncast2 <- reshape::cast(neutral[neutral$LocationCode %in% "Float Reach",], id~ AnalyteName + Replicate, value="Result", fun.aggregate='length')
    ncast2$r <- rep(NA, length(ncast2$id))
    ncast2$r <- lapply(1:length(ncast2$id), function(i, d1, d2, d3, t1, t2, t3){
      mean((d1[i]/t1[i]),(d2[i]/t2[i]),(d3[i]/t3[i]))},
      d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
      t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3")
    nspeed <- tapply(ncast2$r, ncast2$id, invisible)
    FL_N_M <- as.numeric(narea)*as.numeric(nspeed[which(names(nspeed) %in% names(narea))])
    names(FL_N_M) <- names(narea)
    FL_N_F <- FL_N_M*0.0283168466
    
  }

  ###Format Results###
  result<-as.data.frame(matrix(NA, ncol=11, nrow=length(union(names(FL_N_M), names(FL_Q_M)))))
  rownames(result)<-union(names(FL_N_M), names(FL_Q_M))
  
  
  result[which(rownames(result)%in%names(FL_N_F)), 1]<-FL_N_F
  result[which(rownames(result)%in%names(FL_N_M)), 2]<-FL_N_M
  result[which(rownames(result)%in%names(FL_Q_F)), 3]<-FL_Q_F
  result[which(rownames(result)%in%names(FL_Q_M)), 4]<-FL_Q_M
  colnames(result)<-c("FL_N_F.result",  "FL_N_M.result",  "FL_Q_F.result",  "FL_Q_M.result",  "FL_F.result",  "FL_M.result")
  
  result$FL_F.result <- unlist(lapply(1:length(result$FL_N_F), FUN=function(i, a, b){
    mean(c(a[i], b[i]), na.rm=T)}, a=result$FL_N_F, b=result$FL_Q_F))
  result$FL_M.result <- unlist(lapply(1:length(result$FL_N_F), FUN=function(i, a, b){
    mean(c(a[i], b[i]), na.rm=T)}, a=result$FL_N_M, b=result$FL_Q_M))
  
  ###Mean and max water velocity###
  velocity_Q <- tapply(tempp[tempp$AnalyteName == "Velocity",]$Result, tempp[tempp$AnalyteName == "Velocity", ]$id, max)
  result[which(rownames(result)%in%names(velocity_Q)), 7] <- velocity_Q

  # do only if neutral method used
  if(exists('ncast2')){
    
    velocity_N <- lapply(1:length(ncast2$id), function(i, d1, d2, d3, t1, t2, t3){
      max((d1[i]/t1[i]),(d2[i]/t2[i]),(d3[i]/t3[i]))},
      d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
      t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3")
    velocity_N <- tapply(unlist(velocity_N), ncast2$id, invisible)

    result[which(rownames(result)%in%names(velocity_N)), 7] <- unlist(velocity_N)
    result[which(rownames(result)%in%names(nspeed)), 9] <- unlist(nspeed)
  
  }
  result[[8]]<- result[[7]]/3.2808399
  
  velocity_QM <- tapply(tempp[tempp$AnalyteName == "Velocity", ]$Result, tempp[tempp$AnalyteName == "Velocity", ]$id, mean)
  result[which(rownames(result)%in%names(velocity_QM)), 9] <- velocity_QM
  result[[10]]<-result[[9]]/3.2808399
  
  ###Percent zero velocity###
  zerov1 <- tapply(tempp[tempp$AnalyteName == "Velocity", ]$Result, tempp[tempp$AnalyteName == "Velocity", ]$id, 
                   function(data)sum(data==0))
  lengthna <- function(data){sum(!is.na(data))}
  zerov<- (zerov1/tapply(tempp[tempp$AnalyteName == "Velocity", ]$Result, tempp[tempp$AnalyteName == "Velocity", ]$id,
                         lengthna))*100
  result[which(rownames(result)%in%names(zerov)), 11]<-zerov
  
  # do only if neutral method used
  if(exists('ncast2')){
    
    zeron1<- (lapply(1:length(ncast2$id), function(i, d1, d2, d3, t1, t2, t3){
      sum(((d1[i]/t1[i])==0),((d2[i]/t2[i])==0),((d3[i]/t3[i])==0))},
      d1=ncast2$"Distance, Float_1", d2=ncast2$"Distance, Float_2", d3=ncast2$"Distance, Float_3",
      t1=ncast2$"Float Time_1", t2=ncast2$"Float Time_2", t3=ncast2$"Float Time_3"))
    zeron <- tapply(lapply(zeron1, function(d)d/3), ncast2$id, invisible)
    
    result[which(rownames(result)%in%names(zeron)), 11]<-unlist(zeron)
    
  }

  colnames(result)[7:11] <-c("MWVM_F.result", "MWVM_M.result", "XWV_F.result", "XWV_M.result", "PWVZ.result")

  return(result)
  
}