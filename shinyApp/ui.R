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



ui <- fluidPage(
  navbarPage(
    "App Title",
    
    tabPanel(
      "Drinking Water",
      
      sidebarPanel(
        awesomeRadio(
          inputId = "dw_geography",
          label = "Geography:",
          choices = c(World = "world", `Region | Country` = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel(
          condition = "input.dw_geography == 'region_country'",
          virtualSelectInput(
            inputId = "dw_region_country",
            label = "Select Region | Country:",
            choices = region_country_list,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE
          )
        ),
        
        radioButtons(
          inputId = "dw_region",
          label = "Region:",
          choices = c("National",
                      "Rural",
                      "Urban"),
          selected = "National" 
        ),
        
        sliderInput(
          inputId = "dw_year",
          label = "Range of Year:",
          min = 2000, max = 2020, value = c(2000,2020),
          sep = ""
        )
      ),
      
      mainPanel(
        plotOutput(outputId = "dw_bar_plot"),
        plotOutput(outputId = "dw_line_plot"),
        plotOutput(outputId = "dw_donut_plot"),
        plotOutput(outputId = "dw_map_plot")
      )
    ),
    
    tabPanel(
      "Sanitation",
      
      sidebarPanel(
        awesomeRadio(
          inputId = "s_geography",
          label = "Geography:",
          choices = c(World = "world", `Region | Country` = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel(
          condition = "input.s_geography == 'region_country'",
          virtualSelectInput(
            inputId = "s_region_country",
            label = "Select Region | Country:",
            choices = region_country_list,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE
          )
        ),
        
        radioButtons(
          inputId = "s_region",
          label = "Region:",
          choices = c("National",
                      "Rural",
                      "Urban"),
          selected = "National" 
        ),
        
        sliderInput(
          inputId = "s_year",
          label = "Range of Year:",
          min = 2000, max = 2020, value = c(2000,2020),
          sep = ""
        )
      ),
      
      mainPanel(
        plotOutput(outputId = "s_bar_plot"),
        plotOutput(outputId = "s_line_plot"),
        plotOutput(outputId = "s_donut_plot"),
        plotOutput(outputId = "s_map_plot")
      )
    ),
    
    tabPanel(
      "Hygeine",
      
      sidebarPanel(
        awesomeRadio(
          inputId = "h_geography",
          label = "Geography:",
          choices = c(World = "world", `Region | Country` = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel(
          condition = "input.h_geography == 'region_country'",
          virtualSelectInput(
            inputId = "h_region_country",
            label = "Select Region | Country:",
            choices = region_country_list,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE
          )
        ),
        
        radioButtons(
          inputId = "h_region",
          label = "Region:",
          choices = c("National",
                      "Rural",
                      "Urban"),
          selected = "National" 
        ),
        
        sliderInput(
          inputId = "h_year",
          label = "Range of Year:",
          min = 2000, max = 2020, value = c(2000,2020),
          sep = ""
        )
      ),
      
      mainPanel(
        plotOutput(outputId = "h_bar_plot"),
        plotOutput(outputId = "h_line_plot"),
        plotOutput(outputId = "h_donut_plot"),
        plotOutput(outputId = "h_map_plot")
      )
    ),
    
    tabPanel("Modeling")
  )
)