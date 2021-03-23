

source("./functions.R")
source("./data.R")

var1 <- c("wtcpue", "numcpue")
var2 <- c("bot_temp", "bot_depth", "surf_temp")
var_long <- c("CPUE (kg/ha)", "CPUE (number/ha)", 
               "Bottom Temperature (°C)", "Bottom Depth (m)", "Surface Temperature (°C)")

yr <- unique(dat_cpue$year)
survey <- c("NBS", "EBS", "BS")#, "All")
# common <- "Pacific halibut"
common <- unique(dat_cpue$common)
comb1 <- expand.grid(var1, yr, survey, common)
names(comb1) <- c("var", "yr", "survey", "common")

comb2 <- expand.grid(var2, yr, survey, NA)
names(comb2) <- c("var", "yr", "survey", "common")

comb <- rbind.data.frame(comb1, comb2)
  
comb <- comb %>% 
  dplyr::left_join(., data.frame(var = c(var1, var2), var_long)) %>%
  dplyr::mutate(var = as.character(var), 
                survey = as.character(survey), 
                common = as.character(common)) %>%
  tibble()

plot_list <- list() 

for (i in 1:nrow(comb)){
  
  print(i)
  
  temp <- list(idw = NA, 
               plot = NA, 
               filename = NA, 
               yr = comb$yr[i], 
               survey = comb$survey[i], 
               var = comb$var[i],
               var_long = comb$var_long[i],
               common = comb$common[i])
  
  survey0 <- temp$survey
  if (survey0 == "BS") {
    survey0 <- c("NBS", "EBS")
  }
  
  df <- dat_cpue %>%
    dplyr::filter(year == temp$yr &
          survey %in% survey0) %>%
    dplyr::select("year", "wtcpue", "survey", "surf_temp", "stratum", 
                  "station", "scientific", "numcpue", "longitude", "latitude", 
                  # "datetime", 
                  "common", "bot_temp", "bot_depth", "survey_long", "map_area", 
                  "wtcpue_breaks", "numcpue_breaks", "bot_temp_breaks", "bot_depth_breaks", "surf_temp_breaks")
  
  if (!is.na(temp$common)) {
    df <- df %>%
      dplyr::filter(common == temp$common)
  }
  df <- df[!(is.na(df[,temp$var])),]
  
  
  filename <- paste0(temp$yr, "_", 
                     temp$survey,  "_", 
                     temp$var)
  if (temp$var %in% c("wtcpue", "numcpue")) {
    filename <- paste0(temp$yr, "_", 
                       temp$survey,  "_", 
                       temp$var,  "_", 
                       temp$common)
  }
  temp$filename <- filename
  
  if (nrow(df) > 2 && # if there is no (or very little; 2 is arbirtary) data in this dataset to make the idw with
      ((temp$survey == "BS" & # and if eihter.... if BS and either NBS (common) or EBS are missing
      sum(unique(df$survey) %in% survey0) == 2) ||
      temp$survey == survey0) ) { # or if the survey needed is the one available in df
    
    if (temp$survey %in% "BS"){
      df$map_area <- "bs.all"
    }
    
    breaks <- round(eval(parse(text = df[1, paste0(paste(temp$var), "_breaks")])), digits = 1)
    leg_lab <- as.numeric(trimws(formatC(x = breaks, #as.numeric(quantile(x_scaled)),
                                         digits = 3, #drop0trailing = TRUE,
                                         big.mark = ",")))
    leg_lab <- paste0(c(0, leg_lab[-length(leg_lab)]), " - ", leg_lab)
    # x = NA
    # extrap.box = NA
    # grid.cell = c(0.05, 0.05)
    # in.crs = "+proj=longlat"
    # key.title = "auto"
    # log.transform = FALSE
    # idw.nmax = 4
    # use.survey.bathymetry = TRUE
    # return.continuous.grid = TRUE
    # 
    # COMMON_NAME = df$common
    # LATITUDE = df$latitude
    # LONGITUDE = df$longitude
    # CPUE_KGHA = df[,temp$var]
    # region = df$map_area[1]
    # set.breaks = breaks
    # out.crs = "+proj=longlat +datum=WGS84"

    spp_idw0 <- make_idw_map0(COMMON_NAME = df$common,
                              LATITUDE = df$latitude, 
                              LONGITUDE = df$longitude, 
                              CPUE_KGHA = df[,temp$var], 
                              region = df$map_area[1], 
                              set.breaks = breaks) # , out.crs = "+proj=longlat +datum=WGS84"
    
    # scale colors
    if (temp$var %in% c("wtcpue", "numcpue")) {
      pal <- nmfspalette::nmfs_palette(palette = "seagrass",
                                       reverse = TRUE)(spp_idw0$n.breaks)
      pal <- c((pal[-1]), "white")
      # pal <- pal[-length(pal)]
      # pal <- c("white", RColorBrewer::brewer.pal(9, name = "Greens")[c(2, 4, 6, 8, 9)])
      # pal <- pal[-length(pal)]
      # pal <- c("white", pal)
      pal_lab <- c("No Catch", leg_lab)
    } else {
      pal <- c(viridis::viridis(spp_idw0$n.breaks+1))
      pal <- pal[-1]
      pal <- pal[-length(pal)]
      pal_lab <- leg_lab
    }
    
    spp_idw0 <- spp_idw0 %>%
      akgfmaps::add_map_labels(region = df$map_area[1]) %>%
      akgfmaps::change_fill_color(new.scheme = pal,
                                  show.plot = TRUE)
    
    spp_idw0$plot <- spp_idw0$plot + 
      scale_color_manual(
        name = paste0(ifelse(is.na(temp$common),
                             paste0(temp$yr, " Survey"),
                             paste0(temp$yr, " ", temp$common)),
                      "\n", temp$var_long),
        values  = pal, #levels(x = spp_idw0$extrapolation.grid$var1.pred),
        labels = pal_lab
      )
      
    spp_idw0$plot <- spp_idw0$plot + 
      scale_color_discrete(
        name = paste0(ifelse(is.na(temp$common),
                             paste0(temp$yr, " Survey"),
                             paste0(temp$yr, " ", temp$common)),
                      "\n", temp$var_long),
        # values = (pal), 
        limits  = levels(x = spp_idw0$extrapolation.grid$var1.pred),
        # limits = paste0(c("", rep_len(x = ">", 
        #                               length.out = (length(pal_lab)-1))), 
        #                 gsub(pattern = " - ", 
        #                      replacement = "â€“", 
        #                      x = pal_lab)),
        labels = pal_lab)  
    
    levels(x = spp_idw0$extrapolation.grid$var1.pred) <- c("No Catch", leg_lab)
    
    
      scale_fill_manual(
        name = paste0(ifelse(is.na(temp$common), 
                             paste0(temp$yr, " Survey"), 
                             paste0(temp$yr, " ", temp$common)), 
                      "\n", temp$var_long),
        values = (pal), 
        # breaks = gsub(pattern = " - ", replacement = "â€“", x = pal_lab), 
        labels = pal_lab)  

    
    temp$plot <- spp_idw0$plot
    temp$idw <- spp_idw0$extrapolation.grid
    # ggplot() + geom_stars(data = temp$idw)
    
    a <- c("pdf", "png")
    code_str <- glue::glue("ggsave(filename = paste0(filename, '.{a}'), 
           plot = temp$plot, 
           device = '{a}', 
           path = './maps/', 
           width = 12, 
           height = 9, 
           units = 'in')")
    eval(parse(text = code_str))
    
  }

  # Save yo' work
  plot_list <- c(plot_list, list(temp)) 
  names(plot_list)[i %% 100]<-filename
  
  if ((i %% 100) == 0 ||
      i == nrow(comb)) {
    # diff <- ifelse((i %% 100) == 0, 
    #                100, 
    #                i %% 100)
    plot_list0 <- plot_list#[(i-(diff-1)):i]
    save(plot_list0, file = paste0("./maps/plot_list_",i,".Rdata"))
    
    plot_list <- list()
  }
}


# compile complete file
files<-list.files(path = "./maps/", 
                  pattern = "plot_list_", 
                  full.names = TRUE)
idw_list <- list()
for (i in 1:length(files)) {
  load(files[i])
  idw_list0 <- sapply(plot_list0, "[[", c("idw") )
  names(idw_list0)<-names(plot_list0)
  idw_list <- c(idw_list, idw_list0) 
}

for (i in 1:length(idw_list)){
  if (class(idw_list[i][[1]]) == "stars") {
    idw_list[i][[1]] <- st_transform(x = idw_list[i][[1]], crs = "+proj=longlat +datum=WGS84")
  }
}

# sapply(idw_list, st_transform(x = ., crs = "+proj=longlat +datum=WGS84"))
# 
# purrr::lmap(idw_list, st_transform(x = ., crs = "+proj=longlat +datum=WGS84"))

# idw_list0[77]
# a<-idw_list0[77][[1]]; ggplot() + geom_stars(data = st_transform(a, "+proj=longlat +datum=WGS84"))

save(idw_list, file = paste0("./maps/idw_list.Rdata"))
