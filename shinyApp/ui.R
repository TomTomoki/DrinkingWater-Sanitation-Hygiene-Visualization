library(shinythemes)

ui <- fluidPage(
  navbarPage(
    "Water, Sanitation & Hygiene",
    id = "navbar",
    theme = shinytheme("flatly"), #chosen theme
    
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
        girafeOutput(outputId = "dw_map_plot")
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
      "Hygiene",
      
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