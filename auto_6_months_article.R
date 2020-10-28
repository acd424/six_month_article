auto_6_months_article = function(data,crime,march,april,may,june,july,aug, All = FALSE, graph = TRUE ){
  
  #### the crime line graph with ARIMA prediction and CI plotted.
  
  all_years_data = data %>% filter(Crime.type %in% crime) %>% dplyr::select(Month) %>% table() %>% as_tibble()
  last_two_years_data = all_years_data[37:60,2] %>% pull(n)
  this_years_data = all_years_data[47:60,2] %>% pull(n)
  
  this_years_data_updated = c(this_years_data,march,april, may, june,july,aug)
  last_two_years_data_updated = c(last_two_years_data, march,april, may, june,july,aug)
  
  
  x = all_years_data$n/pop_ests$pop[1:60] # getting the pre-pandemic rates
  
  fitARIMA <- auto.arima(ts(x, frequency = 12), trace=FALSE, stepwise = FALSE, approximation = FALSE)
  

  
  futurVal <- forecast(fitARIMA,h=6, level=c(95.0))
  
  forecast_values = as.vector(futurVal$mean)
  upper_CI = as.vector(futurVal$upper)
  lower_CI = as.vector( futurVal$lower)
  
  forecast_values = c(rep(NA,24), forecast_values) #pad with NAs
  upper_CI = c(rep(NA,24), upper_CI) 
  lower_CI = c(rep(NA,24), lower_CI)
  
  
  y_lim_upper = max(max(upper_CI), max(this_years_data))
  
  actual = last_two_years_data_updated/pop_ests$pop[37:66] ## get actual data rates
 

  Month = factor(c("Mar 18" , "Apr 18", "May 18", "Jun 18", "Jul 18", "Aug 18", "Sep 18", "Oct 18", "Nov 18", "Dec 18",'Jan 19' , "Feb 19",
                   "Mar 19" , "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19", "Nov 19", "Dec 19", 'Jan 20' , "Feb 20",
                   "Mar 20", "Apr 20", "May 20", "Jun 20",'Jul 20', 'Aug 20'), 
                 levels = c("Mar 18" , "Apr 18", "May 18", "Jun 18", "Jul 18", "Aug 18", "Sep 18", "Oct 18", "Nov 18", "Dec 18",'Jan 19' , "Feb 19",
                            "Mar 19" , "Apr 19", "May 19", "Jun 19", "Jul 19", "Aug 19", "Sep 19", "Oct 19", "Nov 19", "Dec 19", 'Jan 20' , "Feb 20",
                            "Mar 20", "Apr 20" , "May 20", "Jun 20", 'Jul 20','Aug 20'))
  
  if(All){crime = 'All'}
  data_to_plot = tibble(Forecast = forecast_values, upper_CI = upper_CI, lower_CI = lower_CI, Month = Month, actual = actual, crime =  crime)
  cols <- c("Actual Data"="black","Forecast"="blue")
  
  if(graph){
  g =  ggplot(data = data_to_plot, aes(x = Month, y = actual, group=1))+
    geom_path() +
    geom_point(shape = 18, size = 1, aes(colour = "Actual Data"))+
    geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.25, colour = 'blue', alpha = 0.5)+
    geom_point(data = data_to_plot,aes(x = Month, y = Forecast),colour = 'blue', fill = "blue", shape = 16)+
    ylim(0,max(y_lim_upper))+
    theme_gray()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 7))+
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5))+
    ylab("Count")+xlab(' ')+
    geom_vline(aes(xintercept = "Feb 20"), data = data_to_plot,colour = "grey50", alpha = 0.5)+
    geom_rect(aes(xmin ="Feb 20" , xmax = Inf), colour = NA, ymin = -Inf, ymax = Inf, alpha = 0.01, data = data_to_plot)+
    scale_colour_manual(name = NULL , values=cols)+
    scale_fill_manual(name = NULL, values = cols)+
    ggtitle(str_to_title(crime))+ theme(legend.position = "none", plot.title = element_text( size = 8, face = "bold"))
  
  return(g)
  }

  
  return(data_to_plot)
}