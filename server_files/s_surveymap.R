#######* Survey Map############
## create static element
output$survey_leaflet <- renderLeaflet({
  
  df0 <- dat_cpue %>%
    dplyr::filter(year == input$year &
                    common == input$spp &
                    survey %in% input$survey)

  a <- leaflet() %>%
    addTiles() %>%
    setView(lat = 56.60, 
            lng = -159.3, 
            zoom = 4.5) 
  
  # ADD STRATUM POLYGON? -----------------
  if (input$stratum) {
    
    df <- df0 %>%
      dplyr::filter(survey %in% input$survey) %>% 
      dplyr::select(survey, survey_num, stratum_shp) %>% 
      unique()
    
    code_str <- glue::glue('a <- a %>%
        addPolygons(data = {df$stratum_shp}, 
                    weight = 1, 
                    opacity = 0.5, 
                    stroke = 1, 
                    color = nmfspalette::nmfs_palette(palette = 
                           "oceans")({max(dat_cpue$survey_num)+1})[{df$survey_num}])')
    eval(parse(text = code_str))
  }
  
  
  
  # ADD ENV IDW ------------------
  if (paste(input$env_unit) != "none") {
    
    leg_lab <- as.numeric(trimws(formatC(x = eval(parse(text = df[1, paste0(paste(input$env_unit), "_breaks")])), 
                                         digits = 3, 
                                         big.mark = ",")))
    leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
    
    if (sum(unique(df$survey) %in% c("NBS", "EBS")) == 2) {
      df$map_area[df$survey %in% c("NBS", "EBS")] <- "bs.all"
      df$survey[df$survey %in% c("NBS", "EBS")] <- "BS"
    }
    
    for (i in 1:length(unique(df$survey))) {
      df1 <- df %>% 
        dplyr::filter(survey == unique(df$survey)[i])
      
      pal <- viridis::viridis(n.breaks)
      
      idw0 <- akgfmaps::make_idw_map(COMMON_NAME = df1$common,
                                     LATITUDE = df1$latitude,
                                     LONGITUDE = df1$longitude,
                                     CPUE_KGHA = as.numeric(unlist(df1[,paste(input$env_unit)])),
                                     region = df1$map_area[1],
                                     set.breaks = breaks,
                                     out.crs = "+proj=longlat +datum=WGS84")
      
      idw1 <- idw0$extrapolation.grid
      
      a <- a %>%
        leafem::addStarsImage(x = idw1,
                              colors = pal,
                              opacity = 0.8) 
    }
    
    a <- a %>%
      addLegend(position = "bottomleft", 
                pal = pal, 
                labels = leg_lab, 
                values = as.numeric(breaks), #~df$wtcpue,
                title = paste0(names(input$env_unit)),
                opacity = 0.8)
  }
  
  
  
  # ADD CPUE -------------
  if (paste(input$cpue_unit) != "none") {
    
    df <- df0[df0[,paste(input$cpue_unit)] > 0,]   
    breaks <- eval(parse(text = df[1, paste0(paste(input$cpue_unit), "_breaks")]))
    
    # ***ADD CPUE IDW-------------
    if (paste(input$cpue_display) == "idw") {
      leg_lab <- as.numeric(trimws(formatC(x = breaks, 
                                           digits = 3, 
                                           big.mark = ",")))
      leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
      
      if (sum(unique(df$survey) %in% c("NBS", "EBS")) == 2) {
        df$map_area[df$survey %in% c("NBS", "EBS")] <- "bs.all"
        df$survey[df$survey %in% c("NBS", "EBS")] <- "BS"
      }
      
      for (i in 1:length(unique(df$survey))) {
        
        df1 <- df %>% 
                 dplyr::filter(survey == unique(df$survey)[i])
        
        pal <- nmfspalette::nmfs_palette(palette = "seagrass", reverse = TRUE)(length(breaks)+1)

        # idw0 <- akgfmaps::make_idw_map(COMMON_NAME = df1$common,
        #                                    LATITUDE = df1$latitude,
        #                                    LONGITUDE = df1$longitude,
        #                                    CPUE_KGHA = as.numeric(unlist(df1[,paste(input$cpue_unit)])),
        #                                    region = df1$map_area[1],
        #                                    set.breaks = breaks,
        #                                    out.crs = "+proj=longlat +datum=WGS84")
        # 
        # idw1 <- idw0$extrapolation.grid
        surv <- input$survey
        if (sum(surv %in% c("NBS", "EBS"))>2) {
          surv <- c(surv,"BS")
          surv <- surv[!(surv %in% c("NBS", "EBS"))]
        }
        idwidx1 <- grep(pattern = input$year, 
                        x = names(idw_list), 
                        ignore.case = TRUE)
        idwidx2 <- unique(grep(paste(paste(input$survey),
                                      collapse="|"), 
                                names(idw_list),
                                value=TRUE))
        idwidx3 <- grep(pattern = input$spp, 
                        x = names(idw_list)[1], 
                        ignore.case = TRUE)
        
        
        idw1 <- idw_list[idwidx]
        
        a <- a %>%
          leafem::addStarsImage(x = idw1,
                                colors = pal,
                                opacity = 0.8) 
      }
      
      a <- a %>%
          addLegend(position = "bottomleft", 
                    pal = pal, 
                    labels = leg_lab, 
                    values = as.numeric(breaks), 
                    title = paste0(names(input$cpue_unit)),
                    opacity = 0.8)
      
      # ***ADD CPUE PT-------------
    } else if (paste(input$cpue_display) == "pt") {
      
      df4 <- df %>% 
        dplyr::filter(year == input$year &
                        survey %in% input$survey & 
                        common == input$spp) %>%
        dplyr::select("latitude", "longitude", 
                      "station", "survey", "stratum", "scientific", 
                      "wtcpue", "numcpue", "datetime", "common", 
                      "surf_temp", "bot_temp", "bot_depth")      
      
      circle_size_x <- 5
      x_scaled <- scale_values(as.numeric(unlist(df[,paste(input$cpue_unit)])))+1
      pt_col <- nmfspalette::nmfs_palette(palette = "crustacean")(1)
      leg_lab <- as.numeric(trimws(formatC(x = scale_values(breaks)+1, #as.numeric(quantile(x_scaled)),
                                           digits = 3, #drop0trailing = TRUE,
                                           big.mark = ",")))
      leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
      
      a <- a %>%
        addCircleMarkers(
          data = df4,
          lng = df4$longitude,
          lat = df4$latitude,
          radius = ~ x_scaled*circle_size_x,
          popup = paste("<strong>Species:</strong> ", paste0(df4$common, " (<em>", df4$scientific, "</em>)"), "<br>",
                        # "<strong><u>Survey Data</u></strong> ", "<br>",
                        "<strong>Station:</strong> ", df4$station, "<br>",
                        "<strong>Stratum:</strong> ", df4$stratum,  "<br>",
                        "<strong>Latitude (&degN):</strong> ", df4$latitude,  "<br>",
                        "<strong>Longitude (&degW):</strong> ", df4$longitude,  "<br>",
                        "<strong>Date Surveyed:</strong> ", df4$datetime,  "<br>",
                        # "<strong><u>Environmental Data</u></strong> ", "<br>",
                        "<strong>Bottom Temperature (&degC):</strong> ", df4$bot_temp,  "<br>",
                        "<strong>Surface Temperature (&degC):</strong> ", df4$surf_temp,  "<br>",
                        "<strong>Average Depth (m):</strong> ", df4$bot_depth,  "<br>",
                        # "<strong><u>Species Data</u></strong> ", "<br>",
                        "<strong>Number CPUE (kg of fish/ha):</strong> ", df4$numcpue,  "<br>",
                        "<strong>Weight CPUE (Number of fish/ha):</strong> ", df4$wtcpue, "<br>"), 
          color = pt_col,
          stroke = FALSE,
          fillOpacity = 0.5
        ) %>%
        addLegendCustom(title = paste0("CPUE (", names(input$cpue_unit), ")"), 
                        colors = pt_col, 
                        labels = leg_lab, 
                        sizes = quantile(x_scaled*circle_size_x), 
                        opacity = 0.5)
      
      }

  }
  
 
  # ADD STATION POINTS? ---------------------
  if (input$stat_points) {
    
    df4 <- dat_cpue %>% 
      dplyr::filter(year == input$year &
                      survey %in% input$survey) %>%
      dplyr::select("latitude", "longitude", 
                    "station", "survey", "stratum", "scientific", 
                    "wtcpue", "numcpue", "datetime", "common", 
                    "surf_temp", "bot_temp", "bot_depth")      
    
    
    # df2 <- df4 %>%
    #   dplyr::select("latitude", "longitude", "station") %>%
    #   unique()
    
    a <- a %>%
      addCircleMarkers(
        data = df4,
        lng = df4$longitude,
        lat = df4$latitude, 
        popup = paste("<strong>Station:</strong> ", df4$station, "<br>",
                      "<strong>Stratum:</strong> ", df4$stratum,  "<br>",
                      "<strong>Latitude (&degN):</strong> ", df4$latitude,  "<br>",
                      "<strong>Longitude (&degW):</strong> ", df4$longitude,  "<br>",
                      "<strong>Date Surveyed:</strong> ", df4$datetime,  "<br>",
                      "<strong>Bottom Temperature (&degC):</strong> ", df4$bot_temp,  "<br>",
                      "<strong>Surface Temperature (&degC):</strong> ", df4$surf_temp,  "<br>",
                      "<strong>Average Depth (m):</strong> ", df4$bot_depth,  "<br>"), 
        radius = 2.5, 
        color = nmfspalette::nmfs_palette(palette = "urchin")(1),
        stroke = FALSE, 
        fillOpacity = 0.5) %>%
      addLegend(position = "bottomleft",
                colors = nmfspalette::nmfs_palette(palette = "urchin")(1),
                labels = "Stations",
                # className = "circle",
                opacity = 1)
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
