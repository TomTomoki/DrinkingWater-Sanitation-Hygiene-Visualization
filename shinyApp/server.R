server <- function(input, output){
  values <- reactiveValues(region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL)
  
  observe({
    if (input$navbar == "Drinking Water") {
      values$region <- input$dw_region
      values$year_start <- input$dw_year[1]
      values$year_end <- input$dw_year[2]
      values$map_df <- df_water_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)
          
      output$s_map_plot <- NULL
      output$h_map_plot <- NULL
      
      output$dw_map_plot <- renderGirafe(
        plot_map(values$map_df, "Drinking Water")
      )
    
    } else if (input$navbar == "Sanitation") {
      values$region <- input$s_region
      values$year_start <- input$s_year[1]
      values$year_end <- input$s_year[2]
      values$map_df <- df_sanitation_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (shared)`, Unimproved, `Open defecation`)
      
      output$dw_map_plot <- NULL
      output$h_map_plot <- NULL
      
      output$s_map_plot <- renderGirafe(
        plot_map(values$map_df, "Sanitation")
      )
      
    } else if (input$navbar == "Hygiene") {
      values$region <- input$h_region
      values$year_start <- input$h_year[1]
      values$year_end <- input$h_year[2]
      values$map_df <- df_hygiene_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `Basic`, `Limited (without water or soap)`, `No facility`)
      
      output$dw_map_plot <- NULL
      output$s_map_plot <- NULL
      
      output$h_map_plot <- renderGirafe(
        plot_map(values$map_df, "Hygiene")
      )
    }
  })
}