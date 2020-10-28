all_lsoas = data.frame(all_LSOA = unique(historic_data$LSOA.code))

crime.list2 = "Violence and sexual offences"

crime_decile_data = function(crime, decile_count = 100){

  data_hist_apr = historic_data %>%filter(Crime.type == crime) %>%
    filter(str_detect(Month, '-04')) %>% group_by(LSOA.code) %>% summarise(count = n()) %>% mutate(average = count/5) %>%
    left_join(all_lsoas,., by = c("all_LSOA"="LSOA.code"))%>%
    replace_na(list('count' = 0)) %>% mutate(LSOA.code = all_LSOA)
  
  data_hist_mar = historic_data %>%filter(Crime.type == crime) %>%
    filter(str_detect(Month, '-03')) %>% group_by(LSOA.code) %>% summarise(count = n()) %>% mutate(average = count/5)%>%
    left_join(all_lsoas,., by = c("all_LSOA"="LSOA.code"))%>%
    replace_na(list('count' = 0)) %>% mutate(LSOA.code = all_LSOA)
  
  data_hist_may = historic_data %>%filter(Crime.type == crime) %>%
    filter(str_detect(Month, '-05')) %>% group_by(LSOA.code) %>% summarise(count = n()) %>% mutate(average = count/5)%>%
    left_join(all_lsoas,., by = c("all_LSOA"="LSOA.code"))%>%
    replace_na(list('count' = 0)) %>% mutate(LSOA.code = all_LSOA)
  
  data_hist_june = historic_data %>%filter(Crime.type == crime) %>%
    filter(str_detect(Month, '-06')) %>% group_by(LSOA.code) %>% summarise(count = n()) %>% mutate(average = count/5)%>%
    left_join(all_lsoas,., by = c("all_LSOA"="LSOA.code"))%>%
    replace_na(list('count' = 0)) %>% mutate(LSOA.code = all_LSOA)
  
  data_hist_jul = historic_data %>%filter(Crime.type == crime) %>%
    filter(str_detect(Month, '-07')) %>% group_by(LSOA.code) %>% summarise(count = n()) %>% mutate(average = count/5)%>%
    left_join(all_lsoas,., by = c("all_LSOA"="LSOA.code"))%>%
    replace_na(list('count' = 0)) %>% mutate(LSOA.code = all_LSOA)
  
  
  data_mar = march_data_all%>% filter(Crime.type == crime) %>% group_by(LSOA.code) %>% summarise(count = n())
  
  data_apr = april_data_all%>% filter(Crime.type == crime) %>% group_by(LSOA.code) %>% summarise(count = n())
  
  data_may = may_data_all%>% filter(Crime.type == crime) %>% group_by(LSOA.code) %>% summarise(count = n())
  
  data_june = June_data_all%>% filter(Crime.type == crime) %>% group_by(LSOA.code) %>% summarise(count = n())
  
  data_jul = July_data_all%>% filter(Crime.type == crime) %>% group_by(LSOA.code) %>% summarise(count = n())
  
  
  all_data_apr = left_join(data_hist_apr,data_apr, by = c("LSOA.code" = "LSOA.code"))%>%
    replace_na(list(count.y = 0)) %>% mutate(change = 100*(count.y - average)/average ) %>%
    mutate(abs_change = count.y - average) %>%filter(!LSOA.code == "")
  
  all_data_mar = left_join(data_hist_mar,data_mar, by = c("LSOA.code" = "LSOA.code"))%>%
    replace_na(list(count.y = 0)) %>% mutate(change = 100*(count.y - average)/average ) %>%
    mutate(abs_change = count.y - average) %>%filter(!LSOA.code == "")
  
  all_data_may = left_join(data_hist_may,data_may, by = c("LSOA.code" = "LSOA.code"))%>%
    replace_na(list(count.y = 0)) %>% mutate(change = 100*(count.y - average)/average ) %>%
    mutate(abs_change = count.y - average) %>%filter(!LSOA.code == "")
  
  all_data_june = left_join(data_hist_june,data_june, by = c("LSOA.code" = "LSOA.code"))%>%
    replace_na(list(count.y = 0)) %>% mutate(change = 100*(count.y - average)/average ) %>%
    mutate(abs_change = count.y - average) %>%filter(!LSOA.code == "")
  
  all_data_jul = left_join(data_hist_jul,data_jul, by = c("LSOA.code" = "LSOA.code"))%>%
    replace_na(list(count.y = 0)) %>% mutate(change = 100*(count.y - average)/average ) %>%
    mutate(abs_change = count.y - average) %>%filter(!LSOA.code == "")
  
  decile_data_apr <-all_data_apr %>%filter(complete.cases(.)) %>% 
    mutate(quantile = as.factor(ntile(average, decile_count))) %>%
    select(LSOA.code, average, count.y,change, quantile, abs_change) %>%
    pivot_longer(cols = c(average, count.y, change, abs_change), names_to = 'type')%>% mutate(month = 'April')
  
  decile_data_mar <-all_data_mar %>%filter(complete.cases(.)) %>% 
    mutate(quantile = as.factor(ntile(average, decile_count))) %>%
    select(LSOA.code, average, count.y,change, quantile, abs_change) %>%
    pivot_longer(cols = c(average, count.y, change, abs_change), names_to = 'type')%>% mutate(month = 'March')
  
  
  decile_data_may <-all_data_may %>%filter(complete.cases(.)) %>% 
    mutate(quantile = as.factor(ntile(average, decile_count))) %>%
    select(LSOA.code, average, count.y,change, quantile, abs_change) %>%
    pivot_longer(cols = c(average, count.y, change, abs_change), names_to = 'type')%>% mutate(month = 'May')
  
  decile_data_june <-all_data_june %>%filter(complete.cases(.)) %>% 
    mutate(quantile = as.factor(ntile(average, decile_count))) %>%
    select(LSOA.code, average, count.y,change, quantile, abs_change) %>%
    pivot_longer(cols = c(average, count.y, change, abs_change), names_to = 'type')%>% mutate(month = 'June')
  
  decile_data_jul <-all_data_jul %>%filter(complete.cases(.)) %>% 
    mutate(quantile = as.factor(ntile(average, decile_count))) %>%
    select(LSOA.code, average, count.y,change, quantile, abs_change) %>%
    pivot_longer(cols = c(average, count.y, change, abs_change), names_to = 'type')%>% mutate(month = 'July')
  
  all_decile_data = bind_rows(decile_data_apr, decile_data_mar, decile_data_may,decile_data_june ,decile_data_jul)
 
  return(all_decile_data)
   
}
  