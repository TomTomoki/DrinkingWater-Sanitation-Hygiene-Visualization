#Drinking Water, Sanitation, and Hygiene Household UI
#Team Shabu-shabu, IE6600, Spring 2023

ui <- fluidPage(
  
  #customize font for error message (validation)
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        background-color: white;
        color: darkred;
        font-weight: bold;
      }
    "))
  ), #https://shiny.rstudio.com/articles/validation.html , https://shiny.rstudio.com/articles/css.html 
  
  navbarPage(
    "Water, Sanitation & Hygiene (WASH)",
    id = "navbar",
    theme = shinytheme("flatly"), #chosen theme
    
    ##Summary Tab---------------------------------------------------
    tabPanel(
      "Summary",
      icon = icon("map"),
      # https://fontawesome.com/icons/map?f=sharp&s=regular
      
      sidebarPanel(
        width = 3,
        
        awesomeRadio(
          inputId = "summary_dataset",
          label = "Dataset:",
          choices = c(DrinkingWater = "DrinkingWater", 
                      Sanitation = "Sanitation", 
                      Hygiene = "Hygiene"),
          selected = "DrinkingWater",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        radioButtons(
          inputId = "summary_region",
          label = "Classification Area:",
          choices = c("National",
                      "Rural",
                      "Urban"),
          selected = "National" 
        ),
        
        radioButtons(
          inputId = "summary_year",
          label = "Year:",
          choices = c("2020",
                      "2019",
                      "2018"),
          selected = "2020" 
        )
      ),
      
      mainPanel(
        width = 9,
        wellPanel(
          girafeOutput(outputId = "summary_map_plot"),
          br(),
          br(),
          plotOutput(outputId = "summary_lollipop_plot"),
          downloadButton("download_lollipop", "Download Lollipop Plot", 
                         icon = shiny::icon("download"))
        )
      )
    ),
    
    ##Drinking Water Tab---------------------------------------
    tabPanel(
      "Drinking Water",
      icon = icon("glass-water-droplet"), #e4f4 https://fontawesome.com/icons/glass-water?f=classic&s=solid
       #https://github.com/rstudio/fontawesome 
      
      sidebarPanel(
        helpText("Create WHO/UNICEF JPM household plots based on selected geography, classification area, and year range."),
        
        awesomeRadio(
          inputId = "dw_geography",
          label = "Geography:",
          choices = c(World = "world", 
                      SDGRegion = "region", 
                      Country = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel( #added for Geo Region
          condition = "input.dw_geography == 'region'",
          virtualSelectInput(
            inputId = "dw_geoRegion",
            label = "Select SDG Region:",
            choices = region_list,
            selected = region_list[1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.dw_geography == 'region_country'",
          virtualSelectInput(
            inputId = "dw_region_country",
            label = "Select Country:",
            choices = region_country_list,
            selected = region_country_list[[1]][1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        radioButtons(
          inputId = "dw_region",
          label = "Classification Area:",
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
        wellPanel(
          h3(icon("glass-water-droplet"), "Drinking Water"),
          br(),
          br(),
          plotlyOutput(outputId = "dw_line_plot"),
          br(),
          br(),
          girafeOutput(outputId = "dw_donut_plot"),
          br(),
          br(),
          plotOutput(outputId = "dw_bar_plot"),
          downloadButton("dw_downloadBar", "Download Bar Plot", 
                       icon = shiny::icon("download")),
          br(),
          br(),
          strong("Drinking water service can be classified into 4 levels (from best to worst): "),
          p("- At least basic = 'Drinking water from an improved source, provided collection time is not more than 30 minutes for a round trip, including queuing'"),
          p("- Limited(>30min) = 'Drinking water from an improved source for which collection time exceeds 30 minutes for a round trip, including queuing'"),
          p("- Unimproved = 'Drinking water from an unprotected dug well or unprotected spring'"),
          p("- Surface Water = 'Drinking water directly from a river, dam, lake, pond, stream, canal or irrigation canal'"),
          br(),
          strong("Drinking water supplies can be divided into 2 types: "),
          p("- Piped Supplies = 'Tap water in the dwelling, yard or plot, or public standposts'"),
          p("- Non - Piped Supplies = 'Boreholes / tubewells, or protected wells and springs, or rainwater, or packaged water (including bottled water and sachet water), or delivered water(including tanker trucks and small carts)'")
        )
      )
    ),
    
    ##Sanitation tab -------------------------------------------------------
    tabPanel(
      "Sanitation",
      icon = icon("toilet"), #https://fontawesome.com/icons/toilet?f=classic&s=regular
      
      sidebarPanel(
        helpText("Create WHO/UNICEF JPM household plots based on selected geography, classification area, and year range."),
        
        awesomeRadio(
          inputId = "s_geography",
          label = "Geography:",
          choices = c(World = "world", 
                      SDGRegion = "region", 
                      Country = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel( #added for Geo Region
          condition = "input.s_geography == 'region'",
          virtualSelectInput(
            inputId = "s_geoRegion",
            label = "Select Region:",
            choices = region_list,
            selected = region_list[1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.s_geography == 'region_country'",
          virtualSelectInput(
            inputId = "s_region_country",
            label = "Select Country:",
            choices = region_country_list,
            selected = region_country_list[[1]][1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        radioButtons(
          inputId = "s_region",
          label = "Classification Area:",
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
        wellPanel(
          h3(icon("toilet"), "Sanitation"),
          br(),
          br(),
          plotlyOutput(outputId = "s_line_plot"),
          br(),
          br(),
          girafeOutput(outputId = "s_donut_plot"),
          br(),
          br(),
          plotOutput(outputId = "s_bar_plot"),
          downloadButton("s_downloadBar", "Download Bar Plot", 
                         icon = shiny::icon("download")),
          br(),
          br(),
          strong("Sanitation service can be classified into 4 levels (from best to worst): "),
          p("- At least basic = 'Use of improved facilities that are not shared with other households'"),
          p("- Limited = 'Use of improved facilities shared between two or more households'"),
          p("- Unimproved = 'Use of pit latrines without a slab or platform, hanging latrines or bucket latrines'"),
          p("- Open Defecation = 'Disposal of human faeces in fields, forests, bushes, open bodies of water, beaches or other open spaces, or with solid waste'"),
          br(),
          strong("Sanitation facility types can be divided into 4 types: "),
          p("- Sewer Connection = 'Use sewer connection on premises as sanitation facility'"),
          p("- Septic Tanks = 'Use septic tanks on premises as sanitation facility'"),
          p("- Latrines = 'Use latrines on premises as sanitation facility'"),
          p("- No Facility = 'No sanitation facility on premises'")
        )
      )
    ),
    
    ##Hygiene Tab ------------------------------------------------------
    tabPanel(
      "Hygiene",
      icon = icon("soap"), #https://fontawesome.com/icons/soap?f=classic&s=regular
      
      sidebarPanel(
        helpText("Create WHO/UNICEF JPM household plots based on selected geography, classification area, and year range."),
        
        awesomeRadio(
          inputId = "h_geography",
          label = "Geography:",
          choices = c(World = "world", 
                      SDGRegion = "region", 
                      Country = "region_country"),
          selected = "world",
          inline = TRUE,
          checkbox = TRUE
        ),
        
        conditionalPanel( #added for Geo Region
          condition = "input.h_geography == 'region'",
          virtualSelectInput(
            inputId = "h_geoRegion",
            label = "Select Region:",
            choices = region_list,
            selected = region_list[1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.h_geography == 'region_country'",
          virtualSelectInput(
            inputId = "h_region_country",
            label = "Select Country:",
            choices = region_country_list,
            selected = region_country_list[[1]][1],
            showValueAsTags = TRUE,
            search = TRUE
          )
        ),
        
        radioButtons(
          inputId = "h_region",
          label = "Classification Area:",
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
        wellPanel(
          h3(icon("soap"),"Hygiene"),
          br(),
          br(),
          plotlyOutput(outputId = "h_line_plot"),
          br(),
          girafeOutput(outputId = "h_donut_plot"),
          br(),
          plotOutput(outputId = "h_bar_plot"),
          downloadButton("h_downloadBar", "Download Bar Plot", 
                         icon = shiny::icon("download")),
          br(),
          br(),
          strong("Hygiene service can be classified into 3 levels (from best to worst): "),
          p("- At least basic = 'Availability of a handwashing facility on premises with soap and water'"),
          p("- Limited = 'Availability of handwashing facility on premises without soap and water'"),
          p("- No Facility = 'No handwashing facility on premises'")
          
        )
      )
    ),
    
    ##Forecast Tab -------------------------------------------------
    tabPanel("Forecast",
             icon = icon("arrow-trend-up"), #https://fontawesome.com/icons/arrow-trend-up?f=classic&s=solid
             
             #sidebar Panel
             sidebarPanel(
               width = 3,
               helpText("Ten-year forecast of WHO/UNICEF JPM Household data based on selected geography and classification area."),
               
               awesomeRadio(
                 inputId = "fc_dataset",
                 label = "Dataset:",
                 choices = c(DrinkingWater = "DrinkingWater", 
                             Sanitation = "Sanitation", 
                             Hygiene = "Hygiene"),
                 selected = "DrinkingWater",
                 inline = TRUE,
                 checkbox = TRUE
               ),
               
               awesomeRadio(
                 inputId = "fc_geography",
                 label = "Geography:",
                 choices = c(World = "world", 
                             SDGRegion = "region", 
                             Country = "region_country"),
                 selected = "world",
                 inline = TRUE,
                 checkbox = TRUE
               ),
               
               conditionalPanel( #added for Geo Region
                 condition = "input.fc_geography == 'region'",
                 virtualSelectInput(
                   inputId = "fc_geoRegion",
                   label = "Select SDG Region:",
                   choices = region_list,
                   selected = region_list[1],
                   showValueAsTags = TRUE,
                   search = TRUE
                 )
               ),
               
               conditionalPanel(
                 condition = "input.fc_geography == 'region_country'",
                 virtualSelectInput(
                   inputId = "fc_region_country",
                   label = "Select Country:",
                   choices = region_country_list,
                   selected = region_country_list[[1]][1],
                   showValueAsTags = TRUE,
                   search = TRUE
                 )
               ),
               
               radioButtons(
                 inputId = "fc_region",
                 label = "Classification Area:",
                 choices = c("National",
                             "Rural",
                             "Urban"),
                 selected = "National" 
               ),
             ),
             
             mainPanel(
               width = 9,
               wellPanel(
                 h3(icon("arrow-trend-up"), "2030 Forecast"),
                 
                 p("The forecast graphs display whether JPM's 2030 vision of 100% 'universal access to basic services' will be achieved."),
                 br(),
                 br(),
                 plotOutput(outputId = "ts_raw_plot", width = "75%"),
                 downloadButton("fc_downloadRaw", "Download Decomposed Plot", 
                                icon = shiny::icon("download")),
                 br(),
                 br(),
                 plotOutput(outputId = "ts_forecast_plotES", width = "75%"),
                 downloadButton("fc_downloadForecastES", "Download ES Forecast Plot", 
                                icon = shiny::icon("download")),
                 br(),
                 br(),
                 plotOutput(outputId = "ts_forecast_plotARIMA", width = "75%"),
                 downloadButton("fc_downloadForecastA", "Download ARIMA Forecast Plot", 
                                icon = shiny::icon("download")),
                 br(),
                 br(),
                 h4("Forecast notes:"),
                 p("- The forecasted 2030 'At Least Basic' % for each method is shown in the plot subtitle."),
                 p("- Some countries have flat line values of >99% which is converted to 99.5% for processing. This is why some developed countries seemed to not reach the 2030 goal.")
              )
              )
            ), 
    
    ##About Tab -----------------------------------------------------
    tabPanel("About",
             icon = icon("github"),
             wellPanel(
               includeMarkdown("www/datasets/About.Rmd") #https://shiny.rstudio.com/gallery/biodiversity-national-parks.html 
               )
    )
  )
)