

source("./functions.R")
source("./data.R")

cpue <- c("kg of fish/ha", "number of fish/ha")
yr <- unique(dat_cpue$year)
surv <- c("NBS", "EBS", "BS")#, "All")
common <- unique(dat_cpue$common_name)
sppcode <- unique(dat_cpue$species_code)
comb <- expand.grid(cpue, yr, surv, common, sppcode)
names(comb) <- c("cpue", "yr", "survey", "spp", "sppcode")
comb <- comb %>% 
  dplyr::mutate(map_area = dplyr::case_when(comb$survey %in% "NBS" ~ "bs.north", 
                                  survey %in% "EBS" ~ "bs.south", 
                                  survey %in% c("BS") ~ "bs.all")) %>% # all will change when we have GOA/AI/etc
  dplyr::mutate(cpue = as.character(cpue), 
                survey = as.character(survey), 
                spp = as.character(spp)) %>%
  tibble()



spp_idw <- list() 

for (i in 1:nrow(comb)){
  
  print(i)
  
  temp <- list(idw = NA, 
               plot = NA, 
               yr = comb$yr[i], 
               survey = surv, 
               cpue = comb$cpue[i],
               map_area = comb$map_area[i],
               common_name = comb$spp[i], 
               species_code = comb$sppcode[i])
  
  surv <- comb$survey[i]
  if(comb$survey[i] == "BS") {
    surv <- c("NBS", "EBS")
  }
  
  e <- dat_cpue %>%
    dplyr::select(-file, -area_fished_ha, -stationid, -vessel, -haul) %>%
    dplyr::filter(year == comb$yr[i] &
          survey == surv &
          common_name == comb$spp[i]) 

  
  if (sum(as.numeric(unlist(e[,comb$cpue[i]])), na.rm = T) > 0 &
      nrow(e) != 0 & 
      comb$survey[i] != "NBS" &
      max(as.numeric(unlist(e[,comb$cpue[i]])), na.rm = T) >= 1 # remove anything with less than a max value of 1
      ) {
    
    cpue <- as.numeric(unlist(e[,comb$cpue[i]])) 
    cpue[is.na(cpue)]<-0 # TOLEDO Sean - kosher?
    
    spp_idw0 <- akgfmaps::make_idw_map(COMMON_NAME = e$common_name,
                              LATITUDE = e$latitude, 
                              LONGITUDE = e$longitude, 
                              CPUE_KGHA = cpue, 
                              region = comb$map_area[i], 
                              # key.title = paste0(comb$spp[i], " (", comb$cpue[i], ")"),
                              set.breaks = "jenks", 
                              out.crs = "+proj=longlat +datum=WGS84")
    


    temp$idw <- spp_idw0$extrapolation.grid
    temp$plot <- spp_idw0$plot + 
      guides(fill=guide_legend(title = paste0(comb$spp[i], " (", comb$cpue[i], ")") ))
  }

  # name stuff in your lists
  spp_idw <- c(spp_idw, temp)
  names(spp_idw)[i]<-paste(comb$yr[i], surv, comb$spp[i], comb$cpue[i], sep = "_")

}


