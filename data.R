
SRVY1 <- c("EBS", "NBS")

################## Load Design Based Estimates #################

##### *** Weight ######
df.ls<-list()

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

dat_cpue_newnames <- data.frame(oldnames = c("year", "vessel", "survey", "stratum", 
                                    "stationid", "species_name", "species_code",  
                                    "longitude", "latitude", "haul", "file", 
                                    "cpue_noha", "cpue_kgha", "common_name", "area_fished_ha"), 
                       newnames = c("Year", "Vessel", "Survey", "Stratum", 
                                    "Stationid", "Species Name", "Species Code",  
                                    "Longitude (*W)", "Latitude (*N)", "Haul", "File", 
                                    "CPUE (#/ha)", "CPUE (kg/ha)", "Common Name", "Area Fished (ha)"))

