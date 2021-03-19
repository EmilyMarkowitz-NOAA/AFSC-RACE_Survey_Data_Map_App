#######* Survey Map############
## create static element
output$survey_leaflet <- renderLeaflet({
  e <- df0 %>%
    dplyr::filter(year == input$year &
                    common_name == input$spp &#)
                    # if (input$survey != "All") {
                    #   e <- e %>%
                    # dplyr::filter(
                    survey %in% input$survey)
  # }
  
  a <- leaflet() %>%
    addTiles() %>%
    setView(lat = 56.60, lng = -159.3, zoom = 5) 
  
  # ADD STRATUM POLYGON?
  if (input$stratum) {
    # if (input$survey == "All") {
    #   a <- a %>%
    #     addPolygons(data = bs_shp, 
    #                 weight = 1, 
    #                 color = "grey50", 
    #                 opacity = 0.5)
    # } else 
    if (sum(input$survey %in% "NBS")>0) {
      a <- a %>%
        addPolygons(data = nbs_shp, 
                    weight = 1, 
                    # color = "grey50", 
                    opacity = 0.5, 
                    stroke = 1, 
                    color = nmfspalette::nmfs_palette(palette = "oceans")(1))
    } 
    # else 
    if (sum(input$survey %in% "EBS")>0) {
      a <- a %>%
        addPolygons(data = ebs_shp, 
                    weight = 1, 
                    # color = "grey50", 
                    opacity = 0.5, 
                    stroke = 1, 
                    color = nmfspalette::nmfs_palette(palette = "oceans")(2)[2])
    }
  }
  
  # ADD STATION POINTS?
  if (input$stat_points) {
    a <- a %>%
      # %>%
      addCircleMarkers(
        data = e, 
        lng = e$longitude,
        lat = e$latitude,               
        radius = 1, 
        color = nmfspalette::nmfs_palette(palette = "urchin")(1),
        stroke = FALSE, 
        fillOpacity = 0.5) #%>%
    # addLegend("bottomright", 
    #           pal = nmfspalette::nmfs_palette(palette = "urchin")(1), 
    #           values = 0,
    #           title = paste0("Stations with 0 CPUE (", input$cpue_unit, ")"),
    #           # labFormat = labelFormat(prefix = "$"),
    #           opacity = 1)
  }
  
  # ADD SIZED CPUE?
  if (input$cpue_unit != "None" & input$cpue_points) {
    b <- e[e[,input$cpue_unit] > 0,]
    # b <- e
    # b[is.infinite(b[,input$cpue_unit]),input$cpue_unit] <- 0
    # b <- b[b[,input$cpue_unit] > 0,]
    
    a <- a %>%
      addCircleMarkers(
        data = b,
        lng = b$longitude,
        lat = b$latitude,
        radius = ~ (scale_values(as.numeric(unlist(b[,input$cpue_unit])))+1)*2,
        # label = b[,input$cpue_unit], 
        color = nmfspalette::nmfs_palette(palette = "crustacean")(1),
        stroke = FALSE,
        fillOpacity = 0.5
      )
  }
  
  # ADD IDW
  if (input$cpue_unit != "None") {
    b <- e[e[,input$cpue_unit] > 0,]   
    if (input$cpue_idw){
      
      map_area<-dplyr::case_when(input$survey %in% c("NBS", "EBS") ~ "bs.all",
                                 input$survey == "NBS" ~ "bs.north",
                                 input$survey == "EBS" ~ "bs.south")
      
      spp_idw0 <- akgfmaps::make_idw_map(COMMON_NAME = e$common_name,
                                         LATITUDE = e$latitude,
                                         LONGITUDE = e$longitude,
                                         CPUE_KGHA = as.numeric(unlist(e[,input$cpue_unit])),
                                         region = map_area,
                                         set.breaks = "jenks",
                                         out.crs = "+proj=longlat +datum=WGS84")
      spp_idw <- spp_idw0$extrapolation.grid
      
      a <- a %>%
        leafem::addStarsImage(x = spp_idw,
                              colors = nmfspalette::nmfs_palette(palette = "seagrass")(6),
                              opacity = 0.8) #%>%
      # addLegend(pal = nmfspalette::nmfs_palette(palette = "seagrass")(6),
      #           values = (spp_idw),
      #           title = paste0(input$spp, " (", input$cpue_unit, ")"))
      
    }
    
    
    if (input$bt_idw){
      
      map_area<-dplyr::case_when(input$survey %in% c("NBS", "EBS") ~ "bs.all",
                                 input$survey == "NBS" ~ "bs.north",
                                 input$survey == "EBS" ~ "bs.south")
      
      bt_idw0 <- akgfmaps::make_idw_map(COMMON_NAME = e$common_name,
                                         LATITUDE = e$latitude,
                                         LONGITUDE = e$longitude,
                                         CPUE_KGHA = e$bot_temp,
                                         region = map_area,
                                         set.breaks = "jenks",
                                         out.crs = "+proj=longlat +datum=WGS84")
      bt_idw <- bt_idw0$extrapolation.grid
      
      a <- a %>%
        leafem::addStarsImage(x = bt_idw,
                              colors = viridis::viridis(6),
                              opacity = 0.8) #%>%
      # addLegend(pal = nmfspalette::nmfs_palette(palette = "seagrass")(6),
      #           values = (spp_idw),
      #           title = paste0(input$spp, " (", input$cpue_unit, ")"))
      
    }
    
  }
  
  return(a)
})


output$table <- renderDataTable(input$datasetInput)

# output$table.login <- renderDataTable(DT::renderDT(user_data(), options = list(scrollX = TRUE)))


output$distPlot <- renderPlot({
  
  # generate bins based on input$bins from ui.R
  x    <- as.numeric(data.frame(datasetInput())[, 2]) 
  x <- x[!(is.na(x))]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = input$color, border = 'white')
  
})


output$dl_map <- downloadHandler(
  filename = "survey_map.png",
  
  content = function(file) {
    mapshot(survey_leaflet$dat, file = file)
  }
)
