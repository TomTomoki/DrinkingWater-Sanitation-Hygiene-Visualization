library(ggplot2) #for troubleshooting

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



plot_map <- function(df){
  df <- df %>%
    group_by(`COUNTRY, AREA OR TERRITORY`) %>%
    mutate(avg_basic = mean(`At least basic`, na.rm=TRUE),
           avg_limited = mean(`Limited (more than 30 mins)`, na.rm=TRUE),
           avg_unimproved = mean(Unimproved, na.rm=TRUE),
           avg_surface = mean(`Surface water`, na.rm=TRUE))
  
  plot <- ggplot(df, aes(x = long, y = lat, group = group, fill = avg_basic)) +
    geom_polygon_interactive(aes(tooltip = sprintf("At least basic: %.2f%%, Limited: %.2f%%, Inimproved: %.2f%%, Surface water: %.2f%%", round(avg_basic, 2), round(avg_limited, 2), round(avg_unimproved, 2),round(avg_surface, 2))), 
                             color = 'white') +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) +
    labs(title="Drinking Water Access Level - 2018 ~ 2020",
         x ="",
         y = "")
  
  girafe(ggobj = plot, options = c(opts_hover(css = "cursor:pointer;fill:red;stroke:red;")))
}

plot_line <- function(df, region, year_start, year_end){
  df_line <- df %>%
    filter(toupper(REGION) == toupper(region) & YEAR >= year_start 
           & YEAR <= year_end) %>%
    mutate(Total_Population = `POPULATION(THOUSANDS)` * Percentage 
           / 100 / 1000) %>%
    group_by(YEAR, ServiceLevel) %>%
    summarise(`POPULATION(MILLIONS)` = sum(`Total_Population`, na.rm=TRUE))

  
  ggplot(data = df_line, aes(x=YEAR, y=`POPULATION(MILLIONS)`, 
                             group=ServiceLevel, color=ServiceLevel)) +
    geom_line() +
    geom_point() +
    theme_bw()
}

plot_donut_world <- function(df, region, year_end){
  df_donut <- df %>% 
    filter(ServiceLevel != "AnnualRateOfChangeInBasic" &
             REGION == toupper(region) & 
             YEAR == year_end) %>%  #only specify latest year for clarity
    group_by(ServiceLevel) %>% #combine all country/selected 
    summarise(median = median(Percentage,  na.rm=TRUE)) #insensitive to outlier
  
  hsize <- 3 #donut hole size
  
  ggplot(df_donut, aes(x=hsize, y=median)) +
    geom_col(aes(fill=ServiceLevel))+
    # geom_text(aes(y = median/2 + c(0, cumsum(median)[-length(median)]),
    #               label = paste(round(median,2), "%")),
    #           hjust = 0.5)+
    geom_text(aes(label = paste(round(median,2), "%")), #reduce decimal on value
              position = position_stack(vjust = 0.75),
              hjust = -0.75) +
    coord_polar(theta = "y", start =0)+ #convert bar chart to polar
    # Set the limits, which is important for adding the hole
    xlim(c(0.2, hsize + 0.5))+ 
    scale_fill_brewer(palette = "PuBuGn") + #set ifelse statement?
    theme_void() + # Set theme_void() to remove grid lines and everything else from the plot
    labs(title = "World Service Level Distribution Summary",
         subtitle = "Median Percentage from Countries with Available Values")
}


plot_donut_country <- function(df, region, year_end, country){
  df_donut <- df %>%
    filter(ServiceLevel != "AnnualRateOfChangeInBasic" &
             REGION == toupper(region) & 
             YEAR == year_end & #only specify latest year for clarity
             COUNTRY %in% list(country)) %>% 
    #!is.na(Percentage) %>% 
    group_by(ServiceLevel, COUNTRY) %>% #add country possibilities 
    summarise(median = median(Percentage, na.rm=TRUE)) #remove NA for calc
  
  hsize <- 3 #donut hole size
  
  ggplot(df_donut, aes(x=hsize, y=median)) +
    geom_col(aes(fill=ServiceLevel))+
    geom_text(aes(label = paste(round(median,2), "%")), #reduce decimal on value
              position = position_stack(vjust = 0.75),
              hjust = -0.75) +
    coord_polar(theta = "y", start =0)+ #convert bar chart to polar
    # Set the limits, which is important for adding the hole
    xlim(c(0.2, hsize + 0.5))+ 
    scale_fill_brewer(palette = "PuBuGn") + #set ifelse statement?
    facet_wrap(~COUNTRY)+
    theme_void() + # Set theme_void() to remove grid lines and everything else from the plot
    labs(title = "Country Service Level Distribution Summary")#,
  #subtitle = paste("Year: ", YEAR)) 
}

#check donut function
#plot_donut_world(df_hygiene, "National" , 2020) #this works

#cant work with multiple countries
#plot_donut_country(df_water, "National", 2020, "Zimbabwe") 
#question: Need to use lapply? set country as list in arguement? do.call?
#source: https://gist.github.com/multidis/7995512 

#question: how to add year?

