library(shinythemes)

ui <- fluidPage(
  navbarPage(
    "Water, Sanitation & Hygiene (WASH)",
    id = "navbar",
    theme = shinytheme("flatly"), #chosen theme
    
    tabPanel(
      "Drinking Water",
      
      sidebarPanel(
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
        girafeOutput(outputId = "dw_map_plot"),
        plotOutput(outputId = "dw_line_plot"),
        downloadButton("dw_downloadLine", "Download Line Plot", 
                       icon = shiny::icon("download")),
        plotOutput(outputId = "dw_donut_plot"),
        downloadButton("dw_downloadDonut", "Download Donut Plot", 
                       icon = shiny::icon("download")),
        plotOutput(outputId = "dw_bar_plot"),
        downloadButton("dw_downloadBar", "Download Bar Plot", 
                       icon = shiny::icon("download"))
      )
    ),
    
    tabPanel(
      "Sanitation",
      
      sidebarPanel(
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
        girafeOutput(outputId = "s_map_plot"),
        plotOutput(outputId = "s_line_plot"),
        downloadButton("s_downloadLine", "Download Line Plot", 
                       icon = shiny::icon("download")),
        plotOutput(outputId = "s_donut_plot"),
        downloadButton("s_downloadDonut", "Download Donut Plot", 
                       icon = shiny::icon("download")),
        plotOutput(outputId = "s_bar_plot"),
        downloadButton("s_downloadBar", "Download Bar Plot", 
                       icon = shiny::icon("download"))
      )
    ),
    
    tabPanel(
      "Hygiene",
      
      sidebarPanel(
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
        girafeOutput(outputId = "h_map_plot"),
        plotOutput(outputId = "h_line_plot"),
        downloadButton("h_downloadLine", "Download Line Plot", 
                       icon = shiny::icon("download")),
        
        plotOutput(outputId = "h_donut_plot"),
        downloadButton("h_downloadDonut", "Download Donut Plot", 
                      icon = shiny::icon("download")),
        
        plotOutput(outputId = "h_bar_plot"),
        downloadButton("h_downloadBar", "Download Bar Plot", 
                       icon = shiny::icon("download"))
      )
    ),
    
    tabPanel("Modeling"),
    
    tabPanel("About") 
  )
)