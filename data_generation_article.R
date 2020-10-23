


library(tidyverse)
library(readxl)
library(gridExtra)
library('dotwhisker')
library(gam)
library(FitAR)
library(forecast)
library(visdat)
library(rgdal)
library(tmap)
library(grid)


source('~/Desktop/crime_reports/auto_6_months.R')
source('~/Desktop/crime_reports/grouped_rates_plots.R')

source('~/Desktop/phd material/covid19/Lockdown/leic_5_months.R')
load("crime.list.Rdata")

setwd("~/Desktop/crime_reports")
load('historic_data.Rdata')
load('march_data_all.RData')
load('april_data_all.RData')
load('may_data_all.Rdata')
load('june_data_all.Rdata')
load('july_data_all.Rdata')
load('aug_data_all.Rdata')

results = list()
counter = 1
for( i in as.character(crime.list)){
  
  mar_count = nrow( march_data_all%>% filter(Crime.type == i) %>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  apr_count = nrow(april_data_all%>% filter(Crime.type == i)%>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  may_count = nrow(may_data_all%>% filter(Crime.type == i)%>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  june_count = nrow(June_data_all%>% filter(Crime.type == i)%>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  july_count = nrow(July_data_all%>% filter(Crime.type == i)%>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  aug_count = nrow(aug_data_all%>% filter(Crime.type == i)%>% filter(!Falls.within == 'Police Service of Northern Ireland'))
  
  results[[counter]] = auto_6_months_amend(historic_data, i,
                                          march = mar_count, april = apr_count, may = may_count,
                                          june = june_count,july = july_count,aug = aug_count,
                                          graph = FALSE)
  counter = counter +1
}


nice_crime_names = c('Damage & Arson','Other Theft','ASB','Burglary','Public Order', 'Vehicle Crime', 'Violence & Sex Offences','Other Crime', 'Shoplifting','Bike Theft', 'Drugs','Theft Person','Robbery','Weapon Possession')

###results is output from automated_2_months function
results_df1 = bind_rows(results)

write.csv(results_df1, file = 'nat_patterns_plot_data_aug.csv')
y = c()
for(i in nice_crime_names){
  x = rep(i,30) #increse this each month
  
  y = append(y,x)
  
}

results_df1$nice_crime_names  = y

#### set up the groupings
acq1 = c('Bike Theft','Theft Person','Robbery')
acq2 = c('Other Theft','Burglary', 'Vehicle Crime', 'Shoplifting')
non_acq1 = c('ASB','Violence & Sex Offences')
non_acq2 = c( 'Other Crime',  'Drugs','Weapon Possession')
non_acq3 = c('Damage & Arson','Public Order')

#### set up the colours
x = palette(brewer.pal(n = 7, name = "Dark2"))
set1 = colorRampPalette(x)(15)

cols = c('Damage & Arson' = set1[1],'Other Theft' = set1[2],'ASB' = set1[3],'Burglary'= set1[4],'Public Order'= set1[13], 'Vehicle Crime'= set1[6], 'Violence & Sex Offences'= set1[7],
         'Other Crime'= set1[8], 'Shoplifting'= set1[11],'Bike Theft'= set1[10], 'Drugs'= set1[9],'Theft Person'= set1[15],'Robbery'= set1[5],'Weapon Possession'= set1[14], 'All' = set1[12])

g = grouped_rates_graph(results_df1, acq1,cols)
ggsave(filename = 'acq1_aug.png', width = 9, height = 7)
grouped_rates_graph(results_df1, acq2,cols)
ggsave(filename = 'acq2_aug.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq1,cols)
ggsave(filename = 'non_acq1_aug.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq2,cols)
ggsave(filename = 'non_acq2_aug.png', width = 9, height = 7)
grouped_rates_graph(results_df1, non_acq3,cols)
ggsave(filename = 'non_acq3_aug.png', width = 9, height = 7)


####### All Crime 

crime.list_no_ASB = crime.list[c(1:2,4:14)]

mar_count = nrow( march_data_all%>% filter(Crime.type %in% crime.list_no_ASB ) %>% filter(!Falls.within == 'Police Service of Northern Ireland'))

apr_count = nrow(april_data_all%>% filter(Crime.type %in% crime.list_no_ASB )%>% filter(!Falls.within == 'Police Service of Northern Ireland'))

may_count = nrow(may_data_all%>% filter(Crime.type %in% crime.list_no_ASB )%>% filter(!Falls.within == 'Police Service of Northern Ireland'))

june_count = nrow(June_data_all%>% filter(Crime.type %in% crime.list_no_ASB )%>% filter(!Falls.within == 'Police Service of Northern Ireland'))

july_count = nrow(July_data_all%>% filter(Crime.type %in% crime.list_no_ASB )%>% filter(!Falls.within == 'Police Service of Northern Ireland'))

aug_count = nrow(aug_data_all%>% filter(Crime.type %in% crime.list_no_ASB )%>% filter(!Falls.within == 'Police Service of Northern Ireland'))


g_df = auto_6_months(historic_data, crime.list_no_ASB, march = mar_count, april = apr_count, may = may_count,
                      june = june_count,july = july_count,aug = aug_count,
                     All = TRUE,
                      graph = FALSE)

g_df$nice_crime_names = 'All'
grouped_rates_graph(g_df, 'All',cols)

ggsave(filename = 'all_aug_crime.png', width = 9, height = 7)

save(g_df, file = "all_crime_trend_aug.RData")

######### bar plots


results_df = bind_rows(results) %>% filter(complete.cases(.))%>%
  mutate(percent_change = 100*(actual-Forecast)/Forecast) %>%
  mutate(upper_perc = 100*(actual - upper_CI)/upper_CI) %>%
  mutate(lower_perc = 100*(actual - lower_CI)/lower_CI)

National_trends_aug = results_df
save(National_trends_aug, file = "National_trends_aug.RData")

data_to_plot = results_df %>% filter(Month == 'Jul 20')%>% mutate(percent_change = round(percent_change,1))
data_to_plot$nice_crime = nice_crime_names
data_to_plot = data_to_plot %>% arrange(desc(percent_change))
crime_levels = data_to_plot$nice_crime
data_to_plot$nice_crime = factor(data_to_plot$nice_crime, levels = crime_levels)


ggplot(data =data_to_plot , aes(x = nice_crime, y = percent_change, fill = nice_crime))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle('')+
  xlab('')+
  ylab('% Difference from Expected Rate')+
  theme_gray()+
  ylim(c(-100,200))+
  geom_errorbar(aes(ymin=lower_perc, ymax=upper_perc),
                width=.2, colour = 'blue', alpha = 0.5)+ 
  #geom_text(aes(label=percent_change, hjust = ifelse(percent_change > 0, -0.2,1)),
  #vjust = -0.5,
  #size = 3)+
  theme(legend.position = 'none', legend.title = element_blank())+
  scale_fill_manual(values = cols)+ 
  theme(axis.text.y  = element_text( size = 20,face = 'bold'))+
  theme(axis.text.x  = element_text( size = 17,face = 'bold'))+
  theme(axis.title.x = element_text(size = 20,face = 'bold'))

ggsave(filename = 'ja_july_bar_plot.png')

data_to_plot$CI = paste0('(', round(data_to_plot$upper_perc,1),',',round(data_to_plot$lower_perc,1),')')
write.csv(data_to_plot, file = "ja_july_barplot_data.csv")



data_to_plot = results_df %>% filter(Month == 'Aug 20') %>% mutate(percent_change = round(percent_change,1))
data_to_plot$nice_crime = nice_crime_names
data_to_plot = data_to_plot %>% arrange(desc(percent_change))
crime_levels = data_to_plot$nice_crime
data_to_plot$nice_crime = factor(data_to_plot$nice_crime, levels = crime_levels)


ggplot(data =data_to_plot , aes(x = nice_crime, y = percent_change, fill = nice_crime))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  ggtitle('')+
  xlab('')+
  ylab('% Difference from Expected Rate')+
  theme_gray()+
  ylim(c(-100,200))+
  geom_errorbar(aes(ymin=lower_perc, ymax=upper_perc),
                width=.2, colour = 'blue', alpha = 0.5)+ 
  #geom_text(aes(label=percent_change, hjust = ifelse(percent_change > 0, -0.2,1)),
  #vjust = -0.5,
  #size = 3)+
  theme(legend.position = 'none', legend.title = element_blank())+
  scale_fill_manual(values = cols)+ theme(axis.text.y  = element_text( size = 15))+
  theme(axis.text.y  = element_text( size = 20,face = 'bold'))+
  theme(axis.text.x  = element_text( size = 17,face = 'bold'))+
  theme(axis.title.x = element_text(size = 20,face = 'bold'))

ggsave(filename = 'ja_aug_bar_plot.png')


data_to_plot$CI = paste0('(', round(data_to_plot$upper_perc,1),' , ',round(data_to_plot$lower_perc,1),')')
write.csv(data_to_plot, file = "ja_aug_barplot_data.csv")
