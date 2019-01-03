#' Substrate metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @importFrom magrittr "%>%"
#' 
#' @examples 
#' substrate(sampdat)
substrate <- function(data){
  data <- data[which(data$AnalyteName %in% c('Substrate Size Class', 'Embeddedness', 'CPOM')),]

  data <- data %>%
    dplyr::select(id, LocationCode, AnalyteName, VariableResult, Result) %>%
    unique %>%
    tidyr::complete(id, LocationCode, AnalyteName) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(!all(is.na(VariableResult) & !all(is.na(Result)))) %>%
    dplyr::ungroup()

  data$VariableResult <- as.character(data$VariableResult)
  data$VariableResult[data$VariableResult=="a"]<-"SA"
  data$VariableResult[data$VariableResult=="as"]<-"SA"
  data$VariableResult[data$VariableResult=="ws"]<-"SA"
  data$VariableResult[data$VariableResult=="sn"]<-"SA" 
  data$VariableResult[data$VariableResult=="N"]<-"Not Recorded"
  data$VariableResult[data$VariableResult=="n"]<- "Not Recorded"
  data$VariableResult[data$VariableResult=="LB"]<-"XB"
  data$VariableResult[data$VariableResult=="G"]<- "GC"
  data$VariableResult[data$VariableResult=="FM"]<-"FN"
  data$VariableResult[data$VariableResult=="fd"]<-"SB"
  data$VariableResult[data$VariableResult=="fb"]<-"SB"
  
  sub <- data[data$AnalyteName =="Substrate Size Class",]
  
  sub$VariableResult[sub$Result>= 1000 & sub$Result<4000] <- "XB"
  sub$VariableResult[sub$Result>= 250 & sub$Result<1000] <- "SB"
  sub$VariableResult[sub$Result>= 64 & sub$Result<250] <- "CB"
  sub$VariableResult[sub$Result>= 16 & sub$Result<64] <- "GC"
  sub$VariableResult[sub$Result>= 2 & sub$Result<16] <- "GF"
  sub$VariableResult[sub$Result>= 0.06 & sub$Result<2] <- "SA"
  
  ###Compute
  
  sumna <- function(data){sum(data, na.rm=T)}
  lengthna <- function(data){sum(!is.na(data))}
  
  metric <- c('RS', 'RR', 'RC', 'XB', 'SB', 'CB', 'GC', 'GF', 'SA', 'FN', 'HP', 'WD', 'OT')
  sub$VariableResult <- lapply(sub$VariableResult, toupper)

  lengths <- function(data){
    length(which(((data != "NOT RECORDED") &(data != "NA"))&(data != "FNOT RECORDED")))}
  totals <- tapply(unlist(sub$VariableResult), sub$id, lengths)
  tnames <- as.vector(dimnames(totals))
  qq <-unlist(tnames)
  l <- matrix(NA, ncol=length(metric), nrow=length(totals))
  
  for(j in 1:length(qq))
    for(i in 1:length(metric)){
      l[j, i] <- length(which(sub$VariableResult[sub$id == qq[j]] == metric[i]))
    }
  divd <- function(data) data*100/totals
  result <- as.data.frame(apply(l, 2, divd))
  if(length(colnames(result))==1){
    result <- t(result)
    rownames(result) <- unique(sub$id)
    result <-as.data.frame(result)
  }
  colnames(result) <- c("PCT_RS.result","PCT_RR.result","PCT_RC.result","PCT_XB.result","PCT_SB.result","PCT_CB.result",
                        "PCT_GC.result","PCT_GF.result","PCT_SA.result","PCT_FN.result","PCT_HP.result",
                        "PCT_WD.result","PCT_OT.result")
  
  result$PCT_BDRK.result <- result$PCT_RS + result$PCT_RR
  result$PCT_BIGR.result <- result$PCT_RS + result$PCT_RR + result$PCT_XB + result$PCT_SB + result$PCT_CB + result$PCT_GC
  result$PCT_SFGF.result <- result$PCT_GF + result$PCT_SA + result$PCT_FN
  result$PCT_SAFN.result <- result$PCT_SA + result$PCT_FN

  # H_SubNat, Ev_SubNat
  SubNat <- sub %>% 
    dplyr::select(id, VariableResult) %>% 
    tidyr::unnest() %>% 
    dplyr::group_by(id) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(
      H_SubNat.result = purrr::map(data, function(VariableResult){
  
        VariableResult <- VariableResult %>% dplyr::pull(VariableResult)
        
        # step 2
        RRsum <- sum(VariableResult %in% 'RR')
        RSsum <- sum(VariableResult %in% 'RS')
        HPsum <- sum(VariableResult %in% 'HP')
        
        # step 3
        totsum <- sum(RRsum, RSsum, HPsum)
        if(totsum == 0) 
          return(0)
        
        # step 4
        RRpi <- RRsum / totsum
        RSpi <- RSsum / totsum
        HPpi <- HPsum / totsum
        
        # step 5
        RRpimlt <- RRpi * log(RRpi)
        RSpimlt <- RSpi * log(RSpi)
        HPpimlt <- HPpi * log(HPpi)
        
        # step 6
        res <- (RRpmlt + RSpimlt + HPpimlt) * -1
        
        return(res)
        
        }
      ),
      H_SubNat.count = purrr::map(data, function(VariableResult){
        
        # number of records used for calc
        RRsum <- sum(VariableResult %in% 'RR')
        RSsum <- sum(VariableResult %in% 'RS')
        HPsum <- sum(VariableResult %in% 'HP')
        totsum <- sum(RRsum, RSsum, HPsum)
        
        return(totsum)
        
       }),
      Ev_SubNat.result = purrr::pmap(list(H_SubNat.count, H_SubNat.result), function(H_SubNat.count, H_SubNat.result){
        
        H_SubNat.result / log(H_SubNat.count)
        
      }), 
      EV_SubNat.count = H_SubNat.count
    ) %>% 
    dplyr::select(-data) %>% 
    tidyr::unnest() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    tibble::column_to_rownames('id')

  ###Second set of computation
  sub$value <- rep(NA, length(sub$id))
  sub$value[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
  sub$value[which(sub$VariableResult == "RR")] <- 5660.0
  sub$value[which(sub$VariableResult == "RS")] <- 5660.0
  sub$value[which(sub$VariableResult == "XB")] <- 2500.0
  sub$value[which(sub$VariableResult == "SB")] <- 625.0
  sub$value[which(sub$VariableResult == "CB")] <- 157.0
  sub$value[which(sub$VariableResult == "GC")] <- 40.0
  sub$value[which(sub$VariableResult == "GF")] <- 9.0
  sub$value[which(sub$VariableResult == "SA")] <- 1.03
  sub$value[which(sub$VariableResult == "FN")] <- 0.03
  sub$value[which(sub$VariableResult == "HP")] <- 5660.0
  sub$value[which(sub$VariableResult == "RC")] <- 5660.0
  
  
  
  sub$log <- log10(sub$value)
  
  XSDGM_sum <- tapply(sub$log, sub$id, sumna)
  XSDGM_count <- tapply(sub$log, sub$id, lengthna)
  XSDGM <- 10^(XSDGM_sum/XSDGM_count)
  result$XSDGM.result <- XSDGM
  
  sub$value2 <- rep(NA, length(sub$id))
  sub$value2[which(!is.na(sub$Result))] <- sub$Result[which(!is.na(sub$Result))]
  sub$value2[which(sub$VariableResult == "RR")] <- NA
  sub$value2[which(sub$VariableResult == "RS")] <- NA
  sub$value2[which(sub$VariableResult == "XB")] <- 2500.0
  sub$value2[which(sub$VariableResult == "SB")] <- 625.0
  sub$value2[which(sub$VariableResult == "CB")] <- 157.0
  sub$value2[which(sub$VariableResult == "GC")] <- 40.0
  sub$value2[which(sub$VariableResult == "GF")] <- 9.0
  sub$value2[which(sub$VariableResult == "SA")] <- 1.03
  sub$value2[which(sub$VariableResult == "FN")] <- 0.03
  sub$value2[which(sub$VariableResult == "HP")] <- NA
  sub$value2[which(sub$VariableResult == "RC")] <- NA
  
  
  
  sub$log2 <- log10(sub$value2)
  
  XSPDGM_sum <- tapply(sub$log2, sub$id, sumna)
  XSPDGM_count <- tapply(sub$log2, sub$id, lengthna)
  XSDPGM <- 10^(XSPDGM_sum/XSPDGM_count)
  result$XSPGM.result <- XSDPGM
  
  qant <- function(data)quantile(data, c(.5, .1, .25, .75, .9), na.rm=T)
  otwd <- which(!(sub$VariableResult %in% c("OT", "WD")))
  temp <-tapply(sub$value[otwd], sub$id[otwd], qant)
  for(i in 1:length(temp)){
    result$SB_PT_D50.result[i] <- temp[[i]][1]
    result$SB_PT_D10.result[i] <- temp[[i]][2]
    result$SB_PT_D25.result[i] <- temp[[i]][3]
    result$SB_PT_D75.result[i] <- temp[[i]][4]
    result$SB_PT_D90.result[i] <- temp[[i]][5]
  }
  OTWDHP <- which(!(sub$VariableResult %in% c("OT", "WD", "HP", "RS", "RR", "RC")))
  temp2 <-tapply(sub$value[OTWDHP], sub$id[OTWDHP], qant)
  for(i in 1:length(temp2)){
    result$SB_PP_D50.result[i] <- temp2[[i]][1]
    result$SB_PP_D10.result[i] <- temp2[[i]][2]
    result$SB_PP_D25.result[i] <- temp2[[i]][3]
    result$SB_PP_D75.result[i] <- temp2[[i]][4]
    result$SB_PP_D90.result[i] <- temp2[[i]][5]
  }
  sdna <- function(data) sd(data, na.rm=T)
  embed <- data[which((data$AnalyteName=="Embeddedness")&
                             (!(data$LocationCode=="X"))),]
  XEMBED_sum <- tapply(embed$Result, embed$id, sumna)
  XEMBED_count <- tapply(embed$Result, embed$id, lengthna)
  result$XEMBED.result <- XEMBED_sum/XEMBED_count
  result$XEMBED.count <- XEMBED_count
  result$XEMBED.sd <- tapply(embed$Result, embed$id, sdna)

  cpom <- data[data$AnalyteName=="CPOM",]
  present <- function(data){
    sum(data == "Present", na.rm = TRUE)
  }
  cpomtotal <- function(data){
    sum((data == "Present") | (data == "Absent"), na.rm = TRUE)
  }
  cpresent <- tapply(cpom$VariableResult, cpom$id, present)
  ctotal <- tapply(cpom$VariableResult, cpom$id, cpomtotal)
  result$PCT_CPOM.result <- cpresent*100/ctotal
  
  # add H_SubNat, Ev_SubNat
  result$H_SubNat.result <- SubNat$H_SubNat.result
  result$H_SubNat.count <- SubNat$H_SubNat.count
  result$Ev_SubNat.result <- SubNat$Ev_SubNat.result
  result$Ev_SubNat.count <- SubNat$Ev_SubNat.count
  
  return(result)
  
}