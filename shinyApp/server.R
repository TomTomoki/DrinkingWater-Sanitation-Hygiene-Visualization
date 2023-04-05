server <- function(input, output){
  
  #hyperlink for "About" tabPanel
  observeEvent(input$navbar, {
    if(input$navbar =="About"){
      browseURL("https://github.com/TomTomoki/DrinkingWater-Sanitation-Hygiene-Visualization")
    }
  })
  
  #creating plots
  values <- reactiveValues(geo = NULL,
                           geoRegion = NULL,
                           country = NULL,
                           region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL,
                           df = NULL,
                           df2 = NULL,
                           geoTitle = NULL)
  
  observe({
    if (input$navbar == "Drinking Water") {
      values$geo <- input$dw_geography
      values$region <- input$dw_region
      values$year_start <- input$dw_year[1]
      values$year_end <- input$dw_year[2]
      
      values$map_df <- df_water_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)
          
      #plotting map
      #output$s_map_plot <- NULL
      #output$h_map_plot <- NULL
      output$dw_map_plot <- renderGirafe(
        plot_map(values$map_df, "Drinking Water")
      )
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$dw_geoRegion
        values$df <- df_water %>% filter(SDGRegion == values$geoRegion)
        values$df2 <- df_water2 %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$dw_geoRegion #for plot title
        print(values$geoRegion) #troubleshoot
        print(values$geoTitle)
      } else if(values$geo == "region_country"){
        values$country <- input$dw_region_country
        values$df <- df_water %>% filter(COUNTRY == values$country)
        values$df2 <- df_water2 %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$dw_region_country #for plot title
        print(values$country)  #troubleshoot
        print(values$geoTitle)
      } else {
        values$df <- df_water
        values$df2 <- df_water2
        values$geoTitle <- values$geo
        print(values$geoTitle)
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$dw_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      output$dw_downloadLine <- downloadHandler(
        filename = function() {
          paste("plot_line_dw_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_line(values$df, values$region, values$year_start, values$year_end))
          dev.off()
        }
      )
      
      #plotting bar
      # output$dw_bar_plot <- 
      
      # output$dw_downloadBar <- downloadHandler(
      #   filename = function() {
      #     paste("plot_bar_dw_", values$geoTitle, Sys.Date(), ".png", sep="")
      #   },
      #   content = function(file) {
      #     png(file=file)
      #     plot(plot_bar())#to be entered
      #     dev.off()
      #   }
      # )
      
      #plotting donut chart
      output$dw_donut_plot <- renderPlot({
        plot_donut(values$df, values$region, values$year_end, values$geoTitle)
      })
      
      output$dw_downloadDonut <- downloadHandler(
        filename = function() {
          paste("plot_donut_dw_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_donut(values$df, values$region, values$year_end, values$geoTitle))
          dev.off()
        }
      )
    
    } else if (input$navbar == "Sanitation") {
      values$geo <- input$s_geography
      values$region <- input$s_region
      values$year_start <- input$s_year[1]
      values$year_end <- input$s_year[2]
      
      values$map_df <- df_sanitation_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (shared)`, Unimproved, `Open defecation`)
      
      #plotting map
      #output$dw_map_plot <- NULL
      #output$h_map_plot <- NULL
      output$s_map_plot <- renderGirafe(
        plot_map(values$map_df, "Sanitation")
      )
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$s_geoRegion
        values$df <- df_sanitation %>% filter(SDGRegion == values$geoRegion)
        values$df2 <- df_sanitation2 %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$s_geoRegion #for plot title
        print(values$geoRegion) #troubleshoot
        print(values$geoTitle) #troubleshoot
        
      } else if(values$geo == "region_country"){
        values$country <- input$s_region_country
        values$df <- df_sanitation %>% filter(COUNTRY == values$country)
        values$df2 <- df_sanitation2 %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$s_region_country #for plot title
        print(values$country)  #troubleshoot
        print(values$geoTitle) #troubleshoot
      } else {
        values$df <- df_sanitation
        values$df2 <- df_sanitation2
        values$geoTitle <- values$geo
        print(values$geoTitle) #troubleshoot
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$s_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      output$s_downloadLine <- downloadHandler(
        filename = function() {
          paste("plot_line_san_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_line(values$df, values$region, 
                         values$year_start, values$year_end))
          dev.off()
        }
      )
      
      #plotting bar
      # output$dw_bar_plot <- 
      
      # output$dw_downloadBar <- downloadHandler(
      #   filename = function() {
      #     paste("plot_bar_san_", values$geoTitle, Sys.Date(), ".png", sep="")
      #   },
      #   content = function(file) {
      #     png(file=file)
      #     plot(plot_bar())#to be entered
      #     dev.off()
      #   }
      # )
      
      #plotting donut chart
      output$s_donut_plot <- renderPlot({
        plot_donut(values$df, values$region, values$year_end, values$geoTitle)
      })
      
      output$s_downloadDonut <- downloadHandler(
        filename = function() {
          paste("plot_donut_san_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_donut(values$df, values$region, 
                          values$year_end, values$geoTitle))
          dev.off()
        }
      )
      
      #to be deleted----------------------------
      # #plotting line chart
      # output$s_line_plot <- renderPlot({
      #   plot_line(values$df, values$region, values$year_start, values$year_end)
      # })
      # 
      # #plotting donut chart
      # country <- input$s_region_country #for conditional statement below
      # print(list(input$s_region_country)) #check values for troubleshooting
      # #view(values$df) #for troubleshooting
      # 
      # if(is.null(country)){ #conditional statement
      #   output$s_donut_plot <- renderPlot({
      #     plot_donut_world(values$df, values$region, values$year_end)
      #   })
      # } else {
      #   output$s_donut_plot <- renderPlot({
      #     plot_donut_country(values$df, values$region, values$year_end, country)
      #   }) #CANNOT SHOW faceting?
      # }
      
    } else if (input$navbar == "Hygiene") {
      values$geo <- input$s_geography
      values$region <- input$h_region
      values$year_start <- input$h_year[1]
      values$year_end <- input$h_year[2]
      
      values$map_df <- df_hygiene_map %>%
        filter(Region == values$region & Year >= values$year_start & Year <= values$year_end) %>%
        select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `Basic`, `Limited (without water or soap)`, `No facility`)
      
      #plotting map
      #output$dw_map_plot <- NULL
      #output$s_map_plot <- NULL
      output$h_map_plot <- renderGirafe(
        plot_map(values$map_df, "Hygiene")
      )
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$s_geoRegion
        values$df <- df_hygiene %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$s_geoRegion #for plot title
        
        print(values$geoRegion) #troubleshoot
        print(values$geoTitle) #troubleshoot
        
      } else if(values$geo == "region_country"){
        values$country <- input$s_region_country
        values$df <- df_hygiene %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$s_region_country #for plot title
        
        print(values$country)  #troubleshoot
        print(values$geoTitle) #troubleshoot
      } else {
        values$df <- df_hygiene
        values$geoTitle <- values$geo
        
        print(values$geoTitle) #troubleshoot
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$h_line_plot <- renderPlot({
        plot_line(values$df, values$region, values$year_start, values$year_end)
      })
      
      output$h_downloadLine <- downloadHandler(
        filename = function() {
          paste("plot_line_hyg_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_line(values$df, values$region, values$year_start, values$year_end))
          dev.off()
        }
      )
      
      #plotting bar
      # output$h_bar_plot <- 
      
      # output$h_downloadBar <- downloadHandler(
      #   filename = function() {
      #     paste("plot_bar_hyg_", values$geoTitle, Sys.Date(), ".png", sep="")
      #   },
      #   content = function(file) {
      #     png(file=file)
      #     plot(plot_bar())#to be entered
      #     dev.off()
      #   }
      # )
      
      #plotting donut chart
      output$h_donut_plot <- renderPlot({
        plot_donut(values$df, values$region, values$year_end, values$geoTitle)
      })
      
      output$h_downloadDonut <- downloadHandler(
        filename = function() {
          paste("plot_donut_hyg_", values$geoTitle, Sys.Date(), ".png", sep="")
        },
        content = function(file) {
          png(file=file)
          plot(plot_donut(values$df, values$region, 
                          values$year_end, values$geoTitle))
          dev.off()
        }
      )
      
      #to be deleted----------------------------
      # #plotting line chart
      # output$h_line_plot <- renderPlot({
      #   plot_line(values$df, values$region, values$year_start, values$year_end)
      # })
      # 
      # #plotting donut chart
      # country <- input$h_region_country #for conditional statement below
      # print(list(input$h_region_country)) #check values for troubleshooting
      # #view(values$df) #for troubleshooting
      # 
      # if(is.null(country)){ #conditional statement
      #   output$h_donut_plot <- renderPlot({
      #     plot_donut_world(values$df, values$region, values$year_end)
      #   })
      # } else {
      #   output$h_donut_plot <- renderPlot({
      #     plot_donut_country(values$df, values$region, values$year_end, country)
      #   }) #CANNOT SHOW faceting?
      # }
    }
  })
}
