#' Quality metrics
#'
#' @param data Input data
#' 
#' @export
#' 
#' @examples 
#' quality(sampdat)
quality <- function(data){
  data <- data[which(data$AnalyteName %in% c("Alkalinity as CaCO3", "Oxygen, Dissolved",
                                                      "pH", "Salinity", "SpecificConductivity",
                                                      "Temperature", "Turbidity")),]
  quality_metrics<- data %>% 
                    dplyr::group_by(id) %>%
                    tidyr::nest() %>%
                    dplyr::mutate(
                      XWAK.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == "Alkalinity as CaCO3",]$Result))) %>% round(1)
                      }),
                      XWDO.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName=="Oxygen, Dissolved",]$Result))) %>% round(1)
                      }),
                      XWPH.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == 'pH',]$Result))) %>% round(2)
                      }),
                      XWSL.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == "Salinity",]$Result))) %>% round(1)
                      }),
                      XWSC.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == "SpecificConductivity",]$Result))) %>% round(1)
                      }),
                      XWTC.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == "Temperature",]$Result))) %>% round(1)
                      }),
                      XWTF.result = purrr::map(data, function(df){
                        round(mean(as.numeric(as.character(df[df$AnalyteName == "Temperature",]$Result))) * 1.8 + 32, 1)
                      }),
                      XWTB.result = purrr::map(data, function(df){
                        mean(as.numeric(as.character(df[df$AnalyteName == "Turbidity",]$Result))) %>% round(1)
                      }),
                      XWAK.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "Alkalinity as CaCO3",]$Result))))
                      }),
                      XWDO.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName=="Oxygen, Dissolved",]$Result))))
                      }),
                      XWPH.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == 'pH',]$Result))))
                      }),
                      XWSL.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "Salinity",]$Result))))
                      }),
                      XWSC.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "SpecificConductivity",]$Result))))
                      }),
                      XWTC.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "Temperature",]$Result))))
                      }),
                      XWTF.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "Temperature",]$Result))))
                      }),
                      XWTB.count = purrr::map(data, function(df){
                        sum(!is.na(as.numeric(as.character(df[df$AnalyteName == "Turbidity",]$Result))))
                      })
                    ) %>% dplyr::select(-data) %>% tidyr::unnest() %>% as.data.frame

  rownames(quality_metrics) <- quality_metrics$id
  quality_metrics <- quality_metrics %>% dplyr::select(-id)
  return(quality_metrics)
  
}
