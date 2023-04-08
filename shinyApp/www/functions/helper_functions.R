library(ggplot2) #for troubleshooting

region_list <- c("Central and Southern Asia","Northern America and Europe", 
                 "Western Asia and Northern Africa", "Oceania", 
                 "Sub-Saharan Africa", "Latin America and the Caribbean", 
                 "Australia and New Zealand","Eastern and South-Eastern Asia") 

region_country_list <- list(
  "Central and Southern Asia" = c("Afghanistan", "Bangladesh", "Bhutan", 
                                  "India", "Iran (Islamic Republic of)", 
                                  "Kazakhstan", "Kyrgyzstan", "Maldives", 
                                  "Nepal", "Pakistan", "Sri Lanka", 
                                  "Tajikistan", "Turkmenistan", "Uzbekistan"),
  "Northern America and Europe" = c("Albania", "Andorra",
                                    "Austria", "Belarus", "Belgium", "Bermuda", "Bosnia and Herzegovina", 
                                    "Bulgaria", "Canada", "Channel Islands", "Croatia", "Czech Republic", 
                                    "Denmark", "Estonia", "Faeroe Islands", "Finland", "France", 
                                    "Germany", "Gibraltar", "Greece", "Greenland", "Holy See", "Hungary", 
                                    "Iceland", "Ireland", "Isle of Man", "Italy", "Latvia", "Liechtenstein", 
                                    "Lithuania", "Luxembourg", "Malta", "Monaco", "Montenegro", "Netherlands", 
                                    "North Macedonia", "Norway", "Poland", "Portugal", "Republic of Moldova", 
                                    "Romania", "Russian Federation", "Saint Pierre and Miquelon", 
                                    "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
                                    "Switzerland", "Ukraine", "United Kingdom", "United States of America"),
  "Western Asia and Northern Africa" = c("Algeria", "Armenia", 
                                         "Azerbaijan", "Bahrain", "Cyprus", "Egypt", "Georgia", "Iraq", 
                                         "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", 
                                         "Oman", "Qatar", "Saudi Arabia", "State of Palestine", "Sudan", 
                                         "Syrian Arab Republic", "Tunisia", "Turkey", "United Arab Emirates", 
                                         "Western Sahara", "Yemen"),
  "Oceania" = c("American Samoa", 
                "Cook Islands", "Fiji", "French Polynesia", "Guam", "Kiribati", 
                "Marshall Islands", "Micronesia (Federated States of)", "Nauru", 
                "New Caledonia", "Niue", "Northern Mariana Islands", "Palau", 
                "Papua New Guinea", "Samoa", "Solomon Islands", "Tokelau", "Tonga", 
                "Tuvalu", "Vanuatu", "Wallis and Futuna Islands"),
  "Sub-Saharan Africa" = c("Angola", "Benin", 
                           "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", 
                           "Central African Republic", "Chad", "Comoros", "Congo", "Côte d'Ivoire", 
                           "Democratic Republic of the Congo", "Djibouti", "Equatorial Guinea", 
                           "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                           "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", 
                           "Malawi", "Mali", "Mauritania", "Mauritius", "Mayotte", "Mozambique", 
                           "Namibia", "Niger", "Nigeria", "Réunion", "Rwanda", "Saint Helena", 
                           "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", 
                           "Somalia", "South Africa", "South Sudan", "Togo", "Uganda", "United Republic of Tanzania", 
                           "Zambia", "Zimbabwe"),
  "Latin America and the Caribbean" = c("Anguilla", "Antigua and Barbuda", 
                                        "Argentina", "Aruba", "Bahamas", "Barbados", "Belize", "Bolivia (Plurinational State of)", 
                                        "Brazil", "British Virgin Islands", "Caribbean Netherlands", 
                                        "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", 
                                        "Curaçao", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", 
                                        "Falkland Islands (Malvinas)", "French Guiana", "Grenada", "Guadeloupe", 
                                        "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", 
                                        "Mexico", "Montserrat", "Nicaragua", "Panama", "Paraguay", "Peru", 
                                        "Puerto Rico", "Saint Barthelemy", "Saint Kitts and Nevis", "Saint Lucia", 
                                        "Saint Martin (French part)", "Saint Vincent and the Grenadines", 
                                        "Sint Maarten (Dutch part)", "Suriname", "Trinidad and Tobago", 
                                        "Turks and Caicos Islands", "United States Virgin Islands", "Uruguay", 
                                        "Venezuela (Bolivarian Republic of)"),
  "Australia and New Zealand" = c("Australia", "New Zealand"),
  "Eastern and South-Eastern Asia" = c("Brunei Darussalam", 
                                       "Cambodia", "China", "China, Hong Kong SAR", "China, Macao SAR", 
                                       "Democratic People's Republic of Korea", "Indonesia", "Japan", 
                                       "Lao People's Democratic Republic", "Malaysia", "Mongolia", "Myanmar", 
                                       "Philippines", "Republic of Korea", "Singapore", "Thailand", 
                                       "Timor-Leste", "Viet Nam")
)


plot_map <- function(df, df_type){
  if (df_type == "Drinking Water"){
    df <- df %>%
      group_by(`COUNTRY, AREA OR TERRITORY`) %>%
      mutate(avg_basic = mean(`At least basic`, na.rm=TRUE),
             avg_limited = mean(`Limited (more than 30 mins)`, na.rm=TRUE),
             avg_unimproved = mean(Unimproved, na.rm=TRUE),
             avg_surface = mean(`Surface water`, na.rm=TRUE))
    
    plot <- ggplot(df, aes(x = long, y = lat, group = group, fill = avg_basic)) +
      geom_polygon_interactive(aes(tooltip = sprintf("At least basic: %.2f%%, Limited: %.2f%%, Unimproved: %.2f%%, Surface water: %.2f%%", round(avg_basic, 2), round(avg_limited, 2), round(avg_unimproved, 2),round(avg_surface, 2))), 
                               color = 'white') +
      scale_fill_distiller(palette = "Paired") +
      theme_classic() +
      theme(
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
      labs(title="Drinking Water Access Level",
           x ="",
           y = "",
           fill = "% of At Least Basic Access")
    
  }
  
  else if (df_type == "Sanitation"){
    df <- df %>%
      group_by(`COUNTRY, AREA OR TERRITORY`) %>%
      mutate(avg_basic = mean(`At least basic`, na.rm=TRUE),
             avg_limited = mean(`Limited (shared)`, na.rm=TRUE),
             avg_unimproved = mean(Unimproved, na.rm=TRUE),
             avg_open = mean(`Open defecation`, na.rm=TRUE))
    
    plot <- ggplot(df, aes(x = long, y = lat, group = group, fill = avg_basic)) +
      geom_polygon_interactive(aes(tooltip = sprintf("At least basic: %.2f%%, Limited: %.2f%%, Unimproved: %.2f%%, Open defecation: %.2f%%", round(avg_basic, 2), round(avg_limited, 2), round(avg_unimproved, 2),round(avg_open, 2))), 
                               color = 'white') +
      scale_fill_distiller(palette = "YlOrBr") +
      theme_classic() +
      theme(
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
      labs(title="Sanitation Access Level",
           x ="",
           y = "",
           fill = "% of At Least Basic Access")
    
  }
  
  else if (df_type == "Hygiene"){
    df <- df %>%
      group_by(`COUNTRY, AREA OR TERRITORY`) %>%
      mutate(avg_basic = mean(`Basic`, na.rm=TRUE),
             avg_limited = mean(`Limited (without water or soap)`, na.rm=TRUE),
             avg_noFacility = mean(`No facility`, na.rm=TRUE))
    
    plot <- ggplot(df, aes(x = long, y = lat, group = group, fill = avg_basic)) +
      geom_polygon_interactive(aes(tooltip = sprintf("Basic: %.2f%%, Limited: %.2f%%, No facility: %.2f%%", round(avg_basic, 2), round(avg_limited, 2), round(avg_noFacility, 2))), 
                               color = 'white') +
      scale_fill_distiller(palette = "PuRd") +
      theme_classic() +
      theme(
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
      labs(title="Hygiene Access Level",
           x ="",
           y = "",
           fill = "% of Basic Access")
    
  }
  
  girafe(ggobj = plot, width_svg=8, height_svg=4,
         options = list(
           opts_toolbar(position = "topright"),
           opts_zoom(min = 1, max = 3),
           opts_hover(css = "cursor:pointer;fill:red;stroke:red;")
         ))
}


plot_lollipop <- function(df, df_type, region, year){
  if (df_type == "Drinking Water"){
    df_lollipop <- df %>%
      filter(REGION == toupper(region) & YEAR == year & ServiceLevel == "SurfaceWater") %>%
      arrange(desc(Percentage)) %>%
      top_n(10)
    
    ggplot(df_lollipop, aes(x=fct_inorder(COUNTRY), y=Percentage)) +
      geom_segment(aes(xend=fct_inorder(COUNTRY), y=0, yend=Percentage)) +
      geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
      labs(title = "Top 10 Countries with the Highest % of Populations with SurfaceWater Drinking Water Access",
           x = "Country") +
      theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size=20),
            title = element_text(size=15))
    
  } else if (df_type == "Sanitation"){
    df_lollipop <- df %>%
      filter(REGION == toupper(region) & YEAR == year & ServiceLevel == "OpenDefecation") %>%
      arrange(desc(Percentage)) %>%
      top_n(10)
    
    ggplot(df_lollipop, aes(x=fct_inorder(COUNTRY), y=Percentage)) +
      geom_segment(aes(xend=fct_inorder(COUNTRY), y=0, yend=Percentage)) +
      geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
      labs(title = "Top 10 Countries with the Highest % of Populations with OpenDefecation Sanitation Access",
           x = "Country") +
      theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size=20),
            title = element_text(size=15))
    
  } else if (df_type == "Hygiene"){
    df_lollipop <- df %>%
      filter(REGION == toupper(region) & YEAR == year & ServiceLevel == "NoFacility") %>%
      arrange(desc(Percentage)) %>%
      top_n(10)
    
    ggplot(df_lollipop, aes(x=fct_inorder(COUNTRY), y=Percentage)) +
      geom_segment(aes(xend=fct_inorder(COUNTRY), y=0, yend=Percentage)) +
      geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
      labs(title = "Top 10 Countries with the Highest % of Populations with NoFacility Hygiene Access",
           x = "Country") +
      theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size=20),
            title = element_text(size=15))
  }
}


plot_line <- function(df, region, year_start, year_end){
  df_line <- df %>%
    filter(toupper(REGION) == toupper(region) & YEAR >= year_start 
           & YEAR <= year_end) %>%
    mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage 
           / 100 / 1000) %>%
    group_by(YEAR, ServiceLevel) %>%
    summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE)) %>%
    filter(`POPULATION(MILLIONS)` > 0)

  
  ggplot(data = df_line, aes(x=YEAR, y=`POPULATION(MILLIONS)`, 
                             group=ServiceLevel, color=ServiceLevel)) +
    geom_line() +
    geom_point() +
    theme_bw()
}

plot_donut <- function(df, region, year_end, geo, serviceType, plotColor){
  if(serviceType == "Sanitation"){
    df_donut <- df %>% 
      filter(ServiceLevel != "AnnualRateOfChangeInOpenDefecation")
  } else {
    df_donut <- df
  }
  
  df_donut <- df_donut %>% 
    filter(ServiceLevel != "AnnualRateOfChangeInBasic" &
             REGION == toupper(region) & 
             YEAR == year_end) %>%  #only specify latest year for clarity
    group_by(ServiceLevel) %>% #combine all country/selected 
    summarise(median = median(Percentage,  na.rm=TRUE)) #insensitive to outlier
  
  hsize <- 3 #donut hole size
  
  plot <- ggplot(df_donut, aes(x=hsize, y=median, fill=ServiceLevel))+
    geom_col_interactive(aes(tooltip = paste(ServiceLevel, ": ", round(median,2), "%")))+
    coord_polar(theta = "y")+ #convert bar chart to polar
    # Set the limits, which is important for adding the hole
    xlim(c(0.2, hsize + 0.5))+
    theme_void() +
    scale_fill_brewer(palette = plotColor) +
    labs(title = paste(toupper(geo), 
                       " Service Level Distribution Summary (", 
                       year_end, ")"),
         subtitle = paste(serviceType, 
                          " Median Percentage from Countries with Available Values"))
  
  girafe(ggobj = plot, 
         options = list(
           opts_toolbar(position = "topright", 
                        pngname = paste("plot_donut", serviceType, geo, year_end, sep="_")),
           opts_zoom = opts_zoom(min = 1, max = 3),
           opts_sizing = opts_sizing(rescale = TRUE),
           #opts_hover_inv(css = "opacity:0.1;"),
           opts_hover(css = "cursor:pointer;fill:red;stroke:red;")
           ))
}

#troubleshoot DONUT code
#plot_donut code:
# df_sanitation %>% filter(COUNTRY == "Zimbabwe") %>% 
#     plot_donut("NATIONAL", 2020, "Zimbabwe", "Sanitation", "YlOrBr")
#source: https://www.ardata.fr/ggiraph-book/toolbar.html 

plot_ts_decomp <- function(df, region, geoTitle){
  df_ts <- df %>% 
    filter(ServiceLevel == "AtLeastBasic" &
             REGION == toupper(region))%>%  
    group_by(YEAR) %>% 
    summarise(median = median(Percentage, na.rm = TRUE)) %>% #insensitive to outlier
    ungroup()
  
  ts_forecast <- df_ts %>% ts_ts()
  
  #decomposition
  tsdata_basic <- ts(ts_forecast, frequency = 5) 
  ddata_basic <- decompose(tsdata_basic, "additive") #predicted to be additive (verified)
  
  autoplot(ddata_basic)+
    theme_bw()+
    labs(title = paste(toupper(geoTitle), 
                       "'At least basic' Service Level (", region, ")"),
         subtitle = "Decomposition of Additive Time Series (frequency = 5 years)",
         xlab = "Time (freq=5 years)",
         ylab = "values")
} ##SOURCE: https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r 

#test forecast code:
# df_water %>% filter(COUNTRY == "Zimbabwe") %>%
#        plot_ts_decomp("NATIONAL", "Zimbabwe")

plot_ts_forecast_ES<- function(df, region, geoTitle){
  df_ts <- df %>% 
    filter(ServiceLevel == "AtLeastBasic" &
             REGION == toupper(region))%>%  
    group_by(YEAR) %>% 
    summarise(median = median(Percentage, na.rm = TRUE)) %>% #insensitive to outlier
    ungroup()
  
  ts_forecast <- df_ts %>% ts_ts()
  
  #Exponential smoothing (state space model) 
  ets_model = ets(ts_forecast, allow.multiplicative.trend = TRUE)
  summary(ets_model)
  ets_forecast = forecast(ets_model, h=10) #forecast until 2030
  
  autoplot(ets_forecast, predict.size = 1, 
           predict.colour = 'blue', predict.linetype = 'dashed',
           conf.int = TRUE, conf.int.fill = "lightblue")+
    theme_bw()+
    labs(title = "Time Series Forecast - Exponential Smoothing (ES)",
         subtitle = paste(toupper(geoTitle), "2030 'At least basic' ES forecast =",
                          round(ets_forecast$mean[10], 4), "%"),
         x = "YEAR",
         y = "Median Percentage (%)")
} ##SOURCE: https://towardsdatascience.com/a-guide-to-forecasting-in-r-6b0c9638c261

#test forecast code:
# df_water %>% filter(COUNTRY == "Zimbabwe") %>%
#       plot_ts_forecast_ES("NATIONAL", "Zimbabwe")
   
plot_ts_forecast_ARIMA<- function(df, region, geoTitle){
  df_ts <- df %>% 
    filter(ServiceLevel == "AtLeastBasic" &
             REGION == toupper(region))%>%  
    group_by(YEAR) %>% 
    summarise(median = median(Percentage, na.rm = TRUE)) %>% #insensitive to outlier
    ungroup()
  
  ts_forecast <- df_ts %>% ts_ts()
  
  #AutoArima Model 
  model_arima <- auto.arima(ts_forecast)
  print(model_arima)
  model_arima_fc <- forecast(model_arima, level=c(95), h=10) #forecast until 2030
  
  autoplot(model_arima_fc, predict.size = 1, 
           predict.colour = 'red', predict.linetype = 'dashed',
           conf.int = TRUE, conf.int.fill = "pink")+
    theme_bw()+
    labs(title = "Time Series Forecast - ARIMA",
         subtitle = paste(toupper(geoTitle), 
                          "2030 'At least basic' ARIMA forecast =",
                          round(model_arima_fc$mean[10], 4), "%"),
         x = "YEAR",
         y = "Median Percentage (%)")
} ##SOURCE: https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r#GoTop

#test forecast code:
# df_water %>% #filter(COUNTRY == "Zimbabwe") %>%
#       plot_ts_forecast_ARIMA("NATIONAL", "WORLD")
