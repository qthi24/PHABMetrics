
# import data -------------------------------------------------------------
data <- read_excel('data/RawDataFromCody.xlsx')

data <- data %>% 
  mutate(id = StationCode)

# Codes from phabmetrics.R ------------------------------------------------
data <- phabformat(data)
data <- chkinp(data, purge = TRUE)
metrics <- list(bankmorph(data), channelmorph(data), channelsinuosity(data),
                densiometer(data),  habitat(data), disturbance(data), flow(data),
                misc(data), bankstability(data), quality(data), ripveg(data),
                substrate(data), algae(data))

out <- purrr::map(metrics, function(x){
  
  lnfrm <- x %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    tibble::rownames_to_column('StationCode') %>% 
    dplyr::mutate_if(is.numeric, as.character) %>% 
    tidyr::gather('var', 'val', -StationCode)
  
  return(lnfrm)
  
}) %>% 
  do.call('rbind', .) %>% 
  dplyr::mutate(
    val = gsub('NaN', NA, val)
  ) %>% 
  tidyr::spread('var', 'val') %>% 
  dplyr::mutate_if(
    ~ !any(grepl('[a-z,A-Z]', .x)), as.numeric
  )


# select and gather -------------------------------------------------------

# result ------------------------------------------------------------------

res <- out %>% 
  select(StationCode, contains('result'))
colnames(res) <- sub('.result', '', colnames(res))
res_gather <- res %>% 
  gather('name', 'result', -StationCode)



# standard deviation ------------------------------------------------------

stdv <- out %>% 
  select(StationCode, contains('sd'))
colnames(stdv) <- sub('.sd', '', colnames(stdv))
stdv_gather <- stdv %>% 
  gather('name', 'sd', -StationCode)



# count -------------------------------------------------------------------

counts <- out %>% 
  select(StationCode, contains('count'))
colnames(counts) <- sub('.count', '', colnames(counts))
counts_gather <- counts %>% 
  gather('name', 'count', -StationCode)



# join --------------------------------------------------------------------

test <- full_join(
  res_gather, stdv_gather, by = c('name', 'StationCode')
)

all3 <- full_join(
  test, counts_gather, by = c('name', 'StationCode')
)









