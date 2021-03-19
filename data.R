reg_dat <- get_base_layers(select.region = "bs.all", 
                           set.crs = "+proj=longlat +datum=WGS84")
bs_shp <- reg_dat$survey.strata

reg_dat <- get_base_layers(select.region = "bs.south", 
                           set.crs = "+proj=longlat +datum=WGS84")
ebs_shp <- reg_dat$survey.strata

nbs_shp <- sf::st_read(system.file("data", 
                                   "ebs_strata.shp", 
                                   package = "akgfmaps"), 
                       quiet = TRUE) %>% 
  dplyr::filter(STRATUM %in% c(70, 71, 81)) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")



################## Load Design Based Estimates #################

##### *** Weight ######
df.ls<-list()




SRVY1 <- c("EBS", "NBS")

for (ii in 1:length(SRVY1)) {
  
  a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "CPUE"), full.names = TRUE)
  if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
    a <- a[grep(pattern = "_plusnw", x = a)]
  }
  
  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    b$survey <- SRVY1[ii]
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
}

dat_cpue<-SameColNames(df.ls)

dat_cpue<-dat_cpue %>%
  dplyr::rename("kg of fish/ha" = cpue_kgha, 
                "number of fish/ha" = cpue_noha)

dat_cpue$common_name <- str_to_sentence(dat_cpue$common_name)
df0 <- dat_cpue
df0$`kg of fish/ha`[is.na(df0$`kg of fish/ha`)] <- 0
df0$`number of fish/ha`[is.na(df0$`number of fish/ha`)] <- 0

dat_cpue_newnames <- data.frame(oldnames = c("year", "vessel", "survey", "stratum", 
                                    "stationid", "species_name", "species_code",  
                                    "longitude", "latitude", "haul", "file", 
                                    "cpue_noha", "cpue_kgha", "common_name", "area_fished_ha"), 
                       newnames = c("Year", "Vessel", "Survey", "Stratum", 
                                    "Stationid", "Species Name", "Species Code",  
                                    "Longitude (*W)", "Latitude (*N)", "Haul", "File", 
                                    "CPUE (#/ha)", "CPUE (kg/ha)", "Common Name", "Area Fished (ha)"))

