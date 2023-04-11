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
      geom_polygon_interactive(aes(tooltip = paste(paste(`COUNTRY, AREA OR TERRITORY`),
                                                   paste("At least basic: ", round(avg_basic, 2), "%"), 
                                                   paste("Limited: ", round(avg_limited, 2), "%"), 
                                                   paste("Unimproved: ", round(avg_unimproved, 2), "%"), 
                                                   paste("Surface water: ", round(avg_surface, 2), "%"), 
                                                   sep = "\n")), 
                               color = 'white') +
      scale_fill_distiller(palette = "Blues") +
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
      geom_polygon_interactive(aes(tooltip = paste(paste(`COUNTRY, AREA OR TERRITORY`),
                                                   paste("At least basic: ", round(avg_basic, 2), "%"), 
                                                   paste("Limited: ", round(avg_limited, 2), "%"), 
                                                   paste("Unimproved: ", round(avg_unimproved, 2), "%"), 
                                                   paste("Open defecation: ", round(avg_open, 2), "%"), 
                                                   sep = "\n")), 
                               color = 'white') +
      scale_fill_distiller(palette = "Greens") +
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
      geom_polygon_interactive(aes(tooltip = paste(paste(`COUNTRY, AREA OR TERRITORY`),
                                                   paste("At least basic: ", round(avg_basic, 2), "%"), 
                                                   paste("Limited: ", round(avg_limited, 2), "%"), 
                                                   paste("No facility: ", round(avg_noFacility, 2), "%"),
                                                   sep = "\n")), 
                               color = 'white') +
      scale_fill_distiller(palette = "Oranges") +
      theme_classic() +
      theme(
        axis.line=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
      labs(title="Hygiene Access Level",
           x ="",
           y = "",
           fill = "% of At Least Basic Access")
    
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
      theme_bw() +
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
      theme_bw() +
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
      theme_bw() +
      theme(axis.text.x = element_text(size=15, angle = 45, hjust = 1),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size=20),
            title = element_text(size=15))
  }
}


plot_line <- function(df, region, year_start, year_end, tab){
  if (tab == "Drinking Water" || tab == "Sanitation") {
    df <- df %>%
      filter(ServiceLevel != "AnnualRateOfChangeInBasic")
  }
  
  df_line <- df %>%
    filter(toupper(REGION) == toupper(region) & YEAR >= year_start 
           & YEAR <= year_end)
  
  if (region == "Urban") {
    df_line <- df_line %>%
      mutate(Total_Population = `POPULATION(THOUSANDS)` * (`%URBAN` / 100) 
             * Percentage / 100 / 1000)
  } else if (region == "Rural") {
    df_line <- df_line %>%
      mutate(Total_Population = `POPULATION(THOUSANDS)` * (1 - `%URBAN` / 100) 
             * Percentage / 100 / 1000)
  } else {
    df_line <- df_line %>%
      mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage 
             / 100 / 1000)
  }
  
  df_line <- df_line %>%
    group_by(YEAR, ServiceLevel) %>%
    summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE)) %>%
    filter(`POPULATION(MILLIONS)` > 0)
  
  ggplot(data = df_line, aes(x=YEAR, y=`POPULATION(MILLIONS)`, 
                             group=ServiceLevel, color=ServiceLevel)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(year_start, year_end)) +
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
                       " Service Level Summary (", 
                       year_end, ")"),
         subtitle = paste(serviceType, 
                          " Median % Population (from Countries with Available Values)"))
  
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

plot_bar <- function(df, df_type, year_start, year_end, region) {
  if (df_type == "Drinking Water") {
    df_bar <- df %>%
      filter(toupper(REGION) == toupper(region) & YEAR >= year_start & YEAR <= year_end) %>%
      filter(CriteriaDetails %in% c("Piped", "NonPiped"))
    if (toupper(region) == 'URBAN') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* `%URBAN`/ 100 * Percentage / 100 / 1000)
    } else if (toupper(region) == 'RURAL') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* (100 - `%URBAN`) / 100 * Percentage / 100 / 1000)
    } else {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage / 100 / 1000)
    }
    df_bar <- df_bar %>% 
      group_by(YEAR, CriteriaDetails) %>%
      summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE))
    #print(df_bar)
    df_bar$CriteriaDetails <- factor(df_bar$CriteriaDetails,
                                     levels = c('NonPiped', 'Piped'),
                                     labels = c('Non-Piped', 'Piped'))
    ggplot(df_bar, aes(x = YEAR, y = `POPULATION(MILLIONS)`, fill = CriteriaDetails)) +
      geom_col() +
      geom_text(aes(label = round(`POPULATION(MILLIONS)`, 0), y = `POPULATION(MILLIONS)`, 
                    color = CriteriaDetails), position = position_stack(vjust = 0.5), size = 4) +
      labs(color = "Piped / Non-Piped") +
      scale_fill_manual(values = c('lightblue', 'skyblue')) +
      labs(x = "Year", y = "Total Population (Millions)", fill = "Piped / Non-Piped") +
      theme_bw() +
      theme(text=element_text(size=15)) +
      scale_x_continuous(breaks = seq(min(df_bar$YEAR), max(df_bar$YEAR), 1))
  } else if (df_type == "Sanitation") {
    df_bar <- df %>%
      filter(toupper(REGION) == toupper(region) & YEAR >= year_start & YEAR <= year_end) %>%
      filter(ImprovedSanitationCriteria %in% c("FacilityType"))
    if (toupper(region) == 'URBAN') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* `%URBAN`/ 100 * Percentage / 100 / 1000) %>%
        group_by(YEAR, CriteriaDetails) %>%
        summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE),
                  `POPULATION` = sum(`POPULATION(THOUSANDS)` * `%URBAN`/ 100 / 1000))
    } else if (toupper(region) == 'RURAL') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* (100 - `%URBAN`) / 100 * Percentage / 100 / 1000) %>%
        group_by(YEAR, CriteriaDetails) %>%
        summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE),
                  `POPULATION` = sum(`POPULATION(THOUSANDS)` * (100 - `%URBAN`) / 100 / 1000))
    } else {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage / 100 / 1000) %>%
        group_by(YEAR, CriteriaDetails) %>%
        summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE),
                  `POPULATION` = sum(`POPULATION(THOUSANDS)` / 1000)) 
    }
    
    df_diff <- df_bar %>%
      mutate(difference = `POPULATION`- sum(`POPULATION(MILLIONS)`, na.rm=TRUE)) %>%
      mutate(difference = if_else(is.na(difference) | difference < 0, 0, difference)) %>%
      group_by(YEAR) %>%
      mutate(CriteriaDetails = 'NoFacility') %>%
      summarise(CriteriaDetails = first(CriteriaDetails),
                `POPULATION(MILLIONS)` = mean(`difference`),
                `POPULATION` = mean(`POPULATION`))
    
    df_bar_merge <- rbind(df_bar, df_diff)
    
    df_bar_merge$CriteriaDetails <- factor(df_bar_merge$CriteriaDetails,
                                           levels = c("NoFacility", "LatrinesAndOthers", "SepticTanks", "SewerConnections"), 
                                           labels = c("No Facility", "Latrines", "Septic Tanks", "Sewer Connections"))
    
    #print(df_bar_merge)
    
    ggplot(df_bar_merge, aes(x = YEAR, y = `POPULATION(MILLIONS)`, fill = CriteriaDetails)) +
      geom_col() +
      geom_text(aes(label = round(`POPULATION(MILLIONS)`, 0), y = `POPULATION(MILLIONS)`, 
                    color = CriteriaDetails), position = position_stack(vjust = 0.5), size = 4) +
      labs(color = "Type of Facilities") +
      scale_fill_manual(values = c("#00CC99", "#00BFA5", "#009688", "#00796B")) +
      labs(x = "Year", y = "Total Population (Millions)", fill = "Type of Facilities") +
      theme_bw() +
      theme(text=element_text(size=15)) +
      scale_x_continuous(breaks = seq(min(df_bar_merge$YEAR), max(df_bar_merge$YEAR), 1))
  } else {
    df_bar <- df %>%
      filter(toupper(REGION) == toupper(region) & YEAR >= year_start & YEAR <= year_end) %>%
      filter(ServiceLevel != "AnnualRateOfChangeInBasic") %>%
      filter(ServiceLevel != "Basic")
    if (toupper(region) == 'URBAN') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* `%URBAN`/ 100 * Percentage / 100 / 1000)
    } else if (toupper(region) == 'RURAL') {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)`* (100 - `%URBAN`)/ 100 * Percentage / 100 / 1000)
    } else {
      df_bar <- df_bar %>%
        mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage / 100 / 1000)
    }
    df_bar <- df_bar %>% 
      group_by(YEAR, ServiceLevel) %>%
      summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE))
    
    df_bar$ServiceLevel <- factor(df_bar$ServiceLevel, levels = c("NoFacility", "Limited(withoutWaterOrSoap)"),
                                  labels = c("No Facility", "Limited (Without water or soap)"))
    
    df_bar <- df_bar %>% filter(!is.na(ServiceLevel))
    
    ggplot(df_bar, aes(x = YEAR, y = `POPULATION(MILLIONS)`, fill = ServiceLevel)) +
      geom_col() +
      geom_text(aes(label = round(`POPULATION(MILLIONS)`, 0), y = `POPULATION(MILLIONS)`, 
                    color = ServiceLevel), position = position_stack(vjust = 0.5), size = 4) +
      labs(color = "Service Level") +
      scale_fill_manual(values = c('orange','#CC5500')) +
      labs(x = "Year", y = "Total Population (Millions)", fill = "Service Level") +
      theme_bw() +
      theme(text=element_text(size=15)) +
      scale_x_continuous(breaks = seq(min(df_bar$YEAR), max(df_bar$YEAR), 1))
  }
}


#troubleshoot DONUT code
#plot_donut code:
# df_sanitation %>% filter(COUNTRY == "Zimbabwe") %>% 
#     plot_donut("NATIONAL", 2020, "Zimbabwe", "Sanitation", "YlOrBr")
#source: https://www.ardata.fr/ggiraph-book/toolbar.html 

plot_ts_decomp <- function(df, region, geoTitle){
  df_ts <- df %>%  #tried to filter in server but does not work?
    filter(ServiceLevel == "AtLeastBasic" &
             REGION == toupper(region))%>%  
    group_by(YEAR) %>% 
    summarise(median = median(Percentage, na.rm = TRUE)) %>% #insensitive to outlier
    ungroup()
  
  ts_forecast <- df_ts %>% 
    ts_long %>% 
    ts_ts()
  
  #decomposition
  tsdata_basic <- ts(ts_forecast, frequency = 5) 
  ddata_basic <- decompose(tsdata_basic, "additive") #predicted to be additive (verified)
  
  autoplot(ddata_basic)+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(title = paste(toupper(geoTitle), "(", region,
                       ") 'At least basic' Service Level"),
         subtitle = "Decomposition of Additive Time Series (freq. = 5 years)",
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
  
  ts_forecast <- df_ts %>% 
    ts_long %>% 
    ts_ts()
  
  #Exponential smoothing (state space model) 
  model_holt = holt(ts_forecast)
  summary(model_holt)
  holt_forecast = forecast(model_holt, h=10) #forecast until 2030
  
  #Goal status update:
  target <- round(holt_forecast$mean[10], 4)
  target_label <- "N/A"
  if(target >= 100){
    target_label <- "2030 goal achieved!"
  } else {
    target_label <- "2030 goal not achieved"
  }
  
  autoplot(holt_forecast, predict.size = 1, 
           predict.colour = 'blue', predict.linetype = 'dashed',
           conf.int = TRUE, conf.int.fill = "lightblue")+
    autolayer(fitted(model_holt), series='Predicted')+
    theme_bw()+
    expand_limits(y=100)+
    theme(text=element_text(size=15))+
    #ylim(c(NA, 100))+ #set 100% as max
    labs(title = paste(toupper(geoTitle), region, "Forecast - Exponential Smoothing (Holt)"),
         subtitle = paste("'At least basic' forecast in 2030 =",
                          target, "%, ", target_label),
         x = "YEAR",
         y = "Median Percentage (%) of Population")+
    guides(colour=guide_legend(title=""))+
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill="lightblue",linetype="solid"),
          plot.subtitle = element_text(color = "darkblue"))
} ##SOURCE: https://towardsdatascience.com/a-guide-to-forecasting-in-r-6b0c9638c261
  #source: https://rstudio-pubs-static.s3.amazonaws.com/681477_4af44c0abe0741d8ad5093df109130b5.html 

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
  
  ts_forecast <- df_ts %>% 
    ts_long %>% 
    ts_ts()
  
  #AutoArima Model 
  model_arima <- auto.arima(ts_forecast)
  print(model_arima)
  model_arima_fc <- forecast(model_arima, level=c(95), h=10) #forecast until 2030
  
  #Goal status update:
  target <- round(model_arima_fc$mean[10], 4)
  target_label <- "N/A"
  if(target >= 100){
    target_label <- "2030 goal achieved!"
  } else {
    target_label <- "2030 goal not achieved"
  }
  
  #Plot ARIMA
  autoplot(model_arima_fc, predict.size = 1, 
           predict.colour = 'red', predict.linetype = 'dashed',
           conf.int = TRUE, conf.int.fill = "pink")+
    theme_bw()+
    expand_limits(y=100)+ #increase max y to 100% to standardized
    labs(title = paste(toupper(geoTitle), region, "Forecast - ARIMA (Auto)"),
         subtitle = paste("'At least basic' forecast in 2030=",
                          target, "%, ", target_label),
         x = "YEAR",
         y = "Median Percentage (%) of Population")+
    theme(text=element_text(size=15),
          plot.subtitle = element_text(color = "deeppink2"))
} ##SOURCE: https://www.simplilearn.com/tutorials/data-science-tutorial/time-series-forecasting-in-r#GoTop

#test forecast code:
# df_hygiene %>% filter(COUNTRY == "Indonesia") %>%
#       plot_ts_forecast_ARIMA("NATIONAL", "INDONESIA")
# 
# df_hygiene %>% filter(COUNTRY == "Indonesia") %>%
#   plot_ts_forecast_ES("NATIONAL", "INDONESIA")

#make model reactive? but cant work: https://stackoverflow.com/questions/68241763/r-shiny-error-trying-to-output-reactive-model-summary