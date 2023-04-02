server <- function(input, output){
  values <- reactiveValues(region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL)
  
  values$df <- df_water
  
  # when the user changes tabs
  observeEvent(input$navbar, {
    if (input$navbar == "Drinking Water") {
      values$df <- df_water
    } else if (input$navbar == "Sanitation") {
      values$df <- df_sanitation
    } else if (input$navbar == "Hygiene") {
      values$df <- df_hygiene
    }
  })
  
  
  observe({
    values$region <- input$dw_region
    values$year_start <- input$dw_year[1]
    values$year_end <- input$dw_year[2]
    values$map_df <- df_water_map %>%
      filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
      select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)
    
    if (input$navbar == "Drinking Water") {
      output$dw_map_plot <- renderGirafe(
        plot_map(values$map_df)
      )
      
      output$dw_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
    } else if (input$navbar == "Sanitation") {
      output$s_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
    } else if (input$navbar == "Hygiene") {
      output$h_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
    }
  })
}