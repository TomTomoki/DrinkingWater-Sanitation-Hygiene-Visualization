server <- function(input, output){
  values <- reactiveValues(region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL,
                           df = NULL)
  
  observe({
    if (input$navbar == "Drinking Water") {
      values$df <- df_water
      values$region <- input$dw_region
      values$year_start <- input$dw_year[1]
      values$year_end <- input$dw_year[2]
      values$map_df <- df_water_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)

      #plotting map
      output$dw_map_plot <- renderGirafe(
        plot_map(values$map_df)
      )
      
      #plotting line chart
      output$dw_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      #plotting donut chart
      country <- input$dw_region_country #for conditional statement below
      print(list(input$dw_region_country)) #check values for troubleshooting
      #view(values$df) #for troubleshooting
      
      #conditional statement
      if(is.null(country)){ 
        output$dw_donut_plot <- renderPlot({
          plot_donut_world(values$df, values$region, values$year_end)
        })
      } else {
        output$dw_donut_plot <- renderPlot({
          plot_donut_country(values$df, values$region, values$year_end, country)
        }) #CANNOT SHOW faceting?
      }
      
    } else if (input$navbar == "Sanitation") {
      values$df <- df_sanitation
      values$region <- input$s_region
      values$year_start <- input$s_year[1]
      values$year_end <- input$s_year[2]
      
      #plotting line chart
      output$s_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      #plotting donut chart
      country <- input$s_region_country #for conditional statement below
      print(list(input$s_region_country)) #check values for troubleshooting
      #view(values$df) #for troubleshooting
      
      if(is.null(country)){ #conditional statement
        output$s_donut_plot <- renderPlot({
          plot_donut_world(values$df, values$region, values$year_end)
        })
      } else {
        output$s_donut_plot <- renderPlot({
          plot_donut_country(values$df, values$region, values$year_end, country)
        }) #CANNOT SHOW faceting?
      }
      
    } else if (input$navbar == "Hygiene") {
      values$df <- df_hygiene
      values$region <- input$h_region
      values$year_start <- input$h_year[1]
      values$year_end <- input$h_year[2]
      
      #plotting line chart
      output$h_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      #plotting donut chart
      country <- input$h_region_country #for conditional statement below
      print(list(input$h_region_country)) #check values for troubleshooting
      #view(values$df) #for troubleshooting
      
      if(is.null(country)){ #conditional statement
        output$h_donut_plot <- renderPlot({
          plot_donut_world(values$df, values$region, values$year_end)
        })
      } else {
        output$h_donut_plot <- renderPlot({
          plot_donut_country(values$df, values$region, values$year_end, country)
        }) #CANNOT SHOW faceting?
      }
    }
  })
}


#previous code
#for plotting donut chart
#values$plot_df <- water1 %>% 
# filter(REGION == values$region & 
#         YEAR == values$year_end & #only specify latest year for clarity
#        COUNTRY %in% list(values$country)) 

#view(values$plot_df)