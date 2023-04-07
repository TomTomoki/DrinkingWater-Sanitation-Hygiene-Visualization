server <- function(input, output){
  
  #hyperlink for "About" tabPanel - not used??
  # observeEvent(input$navbar, {
  #   if(input$navbar =="About"){
  #     browseURL("https://github.com/TomTomoki/DrinkingWater-Sanitation-Hygiene-Visualization")
  #   }
  # })
  
  #creating plots
  values <- reactiveValues(geo = NULL,
                           geoRegion = NULL,
                           country = NULL,
                           region = NULL,
                           year_start = NULL,
                           year_end = NULL,
                           map_df = NULL,
                           map_year = NULL,
                           df = NULL,
                           df2 = NULL,
                           geoTitle = NULL,
                           plotColor = NULL,
                           serviceType = NULL)
  
  observe({
    ##Summary tab -------------------------------------------------------
    if (input$navbar == "Summary") {
      values$region <- input$summary_region
      values$map_year <- input$summary_year
      
      if (input$summary_dataset == "DrinkingWater"){
        values$map_df <- df_water_map %>%
          filter(Region == values$region & Year == values$map_year) %>%
          select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (more than 30 mins)`, Unimproved, `Surface water`)
      
        #plotting map
        output$summary_map_plot <- renderGirafe(
          plot_map(values$map_df, "Drinking Water")
        )
        
        #plotting lollipop
        output$summary_lollipop_plot <- renderPlot(
          plot_lollipop(df_water, "Drinking Water", values$region, values$map_year)
        )
      
      } else if (input$summary_dataset == "Sanitation") {
        values$map_df <- df_sanitation_map %>%
          filter(Region == values$region & Year == values$map_year) %>%
          select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `At least basic`, `Limited (shared)`, Unimproved, `Open defecation`)
        
        #plotting map
        output$summary_map_plot <- renderGirafe(
          plot_map(values$map_df, "Sanitation")
        )
        
        #plotting lollipop
        output$summary_lollipop_plot <- renderPlot(
          plot_lollipop(df_sanitation, "Sanitation", values$region, values$map_year)
        )
        
      } else if (input$summary_dataset == "Hygiene") {
        values$map_df <- df_hygiene_map %>%
          filter(Region == values$region & Year == values$map_year) %>%
          select(`COUNTRY, AREA OR TERRITORY`, long, lat, group, `Basic`, `Limited (without water or soap)`, `No facility`)
        
        #plotting map
        output$summary_map_plot <- renderGirafe(
          plot_map(values$map_df, "Hygiene")
        )
        
        #plotting lollipop
        output$summary_lollipop_plot <- renderPlot(
          plot_lollipop(df_hygiene, "Hygiene", values$region, values$map_year)
        )
        
      }
      
    ##Drinking Water tab -------------------------------------------------------
    } else if (input$navbar == "Drinking Water") {
      values$serviceType <- "Drinking Water"
      values$geo <- input$dw_geography
      values$region <- input$dw_region
      values$year_start <- input$dw_year[1]
      values$year_end <- input$dw_year[2]
      values$plotColor <- "Paired"
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$dw_geoRegion
        values$df <- df_water %>% filter(SDGRegion == values$geoRegion)
        values$df2 <- df_water2 %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$dw_geoRegion #for plot title
        # print(values$geoRegion) #for troubleshoot
        # print(values$geoTitle)
      } else if(values$geo == "region_country"){
        values$country <- input$dw_region_country
        values$df <- df_water %>% filter(COUNTRY == values$country)
        values$df2 <- df_water2 %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$dw_region_country #for plot title
        # print(values$country)  #troubleshoot
        # print(values$geoTitle) #for troubleshoot
      } else {
        values$df <- df_water
        values$df2 <- df_water2
        values$geoTitle <- values$geo
        # print(values$geoTitle) #for troubleshoot
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$dw_line_plot <- renderPlotly({
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
      output$dw_donut_plot <- renderGirafe(
        plot_donut(values$df, values$region, values$year_end, 
                   values$geoTitle, values$serviceType, values$plotColor)
      )

    ##Sanitation tab--------------------------------------------
    } else if (input$navbar == "Sanitation") {
      values$serviceType <- "Sanitation"
      values$geo <- input$s_geography
      values$region <- input$s_region
      values$year_start <- input$s_year[1]
      values$year_end <- input$s_year[2]
      values$plotColor <- "YlOrBr"
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$s_geoRegion
        values$df <- df_sanitation %>% filter(SDGRegion == values$geoRegion)
        values$df2 <- df_sanitation2 %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$s_geoRegion #for plot title
        # print(values$geoRegion) #troubleshoot
        # print(values$geoTitle) #troubleshoot
        
      } else if(values$geo == "region_country"){
        values$country <- input$s_region_country
        values$df <- df_sanitation %>% filter(COUNTRY == values$country)
        values$df2 <- df_sanitation2 %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$s_region_country #for plot title
        # print(values$country)  #troubleshoot
        # print(values$geoTitle) #troubleshoot
      } else {
        values$df <- df_sanitation
        values$df2 <- df_sanitation2
        values$geoTitle <- values$geo
        #print(values$geoTitle) #troubleshoot
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$s_line_plot <- renderPlotly({
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
      output$s_donut_plot <- renderGirafe(
        plot_donut(values$df, values$region, values$year_end, 
                   values$geoTitle, values$serviceType, values$plotColor)
      )

    
    ##Hygiene tab -----------------------------------------------------  
    } else if (input$navbar == "Hygiene") {
      values$serviceType <- "Hygiene"
      values$geo <- input$h_geography
      values$region <- input$h_region
      values$year_start <- input$h_year[1]
      values$year_end <- input$h_year[2]
      values$plotColor <- "PuRd"
      
      #conditional statement to filter per region or country, if applicable
      if(values$geo == "region"){
        values$geoRegion <- input$h_geoRegion
        values$df <- df_hygiene %>% filter(SDGRegion == values$geoRegion)
        values$geoTitle <- input$h_geoRegion #for plot title
        
        # print(values$geoRegion) #troubleshoot
        # print(values$geoTitle) #troubleshoot
        
      } else if(values$geo == "region_country"){
        values$country <- input$h_region_country
        values$df <- df_hygiene %>% filter(COUNTRY == values$country)
        values$geoTitle <- input$h_region_country #for plot title
        
        # print(values$country)  #troubleshoot
        # print(values$geoTitle) #troubleshoot
      } else {
        values$df <- df_hygiene
        values$geoTitle <- values$geo
        
        #print(values$geoTitle) #troubleshoot
      }
      #view(values$df) #for troubleshooting
      
      #plotting line chart
      output$h_line_plot <- renderPlotly({
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
      output$h_donut_plot <- renderGirafe(
        plot_donut(values$df, values$region, values$year_end, 
                   values$geoTitle, values$serviceType, values$plotColor)
      )
      
    }
  })
}
