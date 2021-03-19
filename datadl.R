

files <- c(
  # AI
  "ai1983_2000", 
  "ai2002_2012",
  "ai2014_2018",
  
  # BS Slope
  "bsslope2002_2016", 

  # EBS
  "ebs1982_1984", 
  "ebs1985_1989",
  "ebs1990_1994", 
  "ebs1995_1999",
  "ebs2000_2004", 
  "ebs2005_2008",
  "ebs2009_2012", 
  "ebs2013_2016", 
  "ebs2017_2019",
  
  # NBS
  "nbs1982_2019", 
  
  # GOA
  "goa1984_1987", 
  "goa1990_1999",
  "goa2000_2005", 
  "goa2007_2013",
  "goa2015_2019"
  
  )

url <- "https://apps-afsc.fisheries.noaa.gov/RACE/groundfish/survey_data/downloads/"

for (i in 1:length(files)) {
  download.file(url = paste0(url, files[i],".zip"), 
                destfile = paste0("./data/publicdata/zip/", files[i], ".zip"), 
                quiet = TRUE)
  unzip(zipfile = paste0("./data/publicdata/zip/", files[i], ".zip"), 
        overwrite = TRUE, 
        exdir = paste0("./data/publicdata/unzip/", files[i]))
  
  file.copy(from = paste0("./data/publicdata/unzip/", files[i], "/", files[i], ".csv"),
            to = paste0("./data/publicdata/"), 
            overwrite = TRUE)
  
  file.remove(paste0("./data/publicdata/unzip/", files[i], "/", files[i], ".csv"))
  # unlink(x = paste0("./data/publicdata/", files[i], "/"), force = TRUE, recursive = TRUE)
  # do.call(file.remove, list(list.files(paste0("./data/publicdata/", files[i], "/"), full.names = TRUE)))
  
}

df.ls<-list()

SRVY1 <- toupper(unique(sub("^([[:alpha:]]*).*", "\\1", files)))
a<-list.files(path = here::here("data", "publicdata"), 
              full.names = TRUE, 
              pattern = ".csv")

for (i in 1:length(a)){
  b <- read_csv(file = a[i])
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  urlname <- gsub(pattern = ".csv", 
                  replacement = "", 
                  x = urlname[[1]][length(urlname[[1]])])
  b$file <- paste0(url, urlname, ".zip")
  b$survey <- toupper(unique(sub("^([[:alpha:]]*).*", "\\1", urlname)))
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
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



