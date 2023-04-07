#Drinking Water, Sanitation, and Hygiene Household UI
#Team Shabu-shabu, IE6600, Spring 2023

ui <- fluidPage(

  navbarPage(
    "Water, Sanitation & Hygiene (WASH)",
    id = "navbar",
    theme = shinytheme("flatly"), #chosen theme
    
    ##Drinking Water Tab---------------------------------------------------
    tabPanel(
      "Summary",
      
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
        #width = 9,
        
        girafeOutput(outputId = "summary_map_plot")
      )
    ),
    
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
        fluidPage(
          h2("Drinking Water"),
          h4("Drinking water service can be classified into 4 levels (from best to worst): "),
          p("- At least basic = 'Drinking water from an improved source, provided collection time is not more than 30 minutes for a round trip, including queuing'"),
          p("- Limited(>30min) = 'Drinking water from an improved source for which collection time exceeds 30 minutes for a round trip, including queuing'"),
          p("- Unimproved = 'Drinking water from an unprotected dug well or unprotected spring'"),
          p("- Surface Water = 'Drinking water directly from a river, dam, lake, pond, stream, canal or irrigation canal'"),
          br(),
          br(),
          plotOutput(outputId = "dw_line_plot"),
          downloadButton("dw_downloadLine", "Download Line Plot", 
                       icon = shiny::icon("download")),
          br(),
          girafeOutput(outputId = "dw_donut_plot"),
          br(),
          plotOutput(outputId = "dw_bar_plot"),
          downloadButton("dw_downloadBar", "Download Bar Plot", 
                       icon = shiny::icon("download"))
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
        fluidPage(
          h2("Sanitation"),
          h4("Sanitation service can be classified into 4 levels (from best to worst): "),
          p("- At least basic = 'Use of improved facilities that are not shared with other households'"),
          p("- Limited = 'Use of improved facilities shared between two or more households'"),
          p("- Unimproved = 'Use of pit latrines without a slab or platform, hanging latrines or bucket latrines'"),
          p("- Open Defecation = 'Disposal of human faeces in fields, forests, bushes, open bodies of water, beaches or other open spaces, or with solid waste'"),
          br(),
          br(),
          plotOutput(outputId = "s_line_plot"),
          downloadButton("s_downloadLine", "Download Line Plot", 
                         icon = shiny::icon("download")),
          br(),
          br(),
          girafeOutput(outputId = "s_donut_plot"),
          br(),
          br(),
          plotOutput(outputId = "s_bar_plot"),
          downloadButton("s_downloadBar", "Download Bar Plot", 
                         icon = shiny::icon("download"))
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
        fluidPage(
          h2("Hygiene"),
          h4("Hygiene service can be classified into 3 levels (from best to worst): "),
          p("- At least basic = 'Availability of a handwashing facility on premises with soap and water'"),
          p("- Limited = 'Availability of handwashing facility on premises without soap and water'"),
          p("- No Facility = 'No handwashing facility on premises'"),
          br(),
          br(),
          plotOutput(outputId = "h_line_plot"),
          downloadButton("h_downloadLine", "Download Line Plot", 
                         icon = shiny::icon("download")),
          br(),
          girafeOutput(outputId = "h_donut_plot"),
          br(),
          plotOutput(outputId = "h_bar_plot"),
          downloadButton("h_downloadBar", "Download Bar Plot", 
                         icon = shiny::icon("download"))
        )
      )
    ),
    
    ##Forecast Tab -------------------------------------------------
    tabPanel("Forecast",
             icon = icon("arrow-trend-up")), #https://fontawesome.com/icons/arrow-trend-up?f=classic&s=solid
    
    ##About Tab -----------------------------------------------------
    tabPanel("About",
             icon = icon("github"),
             includeMarkdown("www/datasets/About.Rmd") #https://shiny.rstudio.com/gallery/biodiversity-national-parks.html 
               ) 
  )
)