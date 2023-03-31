server <- function(input, output){
  values <- reactiveValues(region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL)
  
  observe({
    values$region <- input$dw_region
    values$year_start <- input$dw_year[1]
    values$year_end <- input$dw_year[2]
    values$map_df <- df_water_map %>%
      filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
      select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)
        
    output$dw_map_plot <- renderGirafe(
      plot_map(values$map_df)
    )
  })
}