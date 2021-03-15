
###########PACKAGES###################
library(devtools)
library(here)

# Need for running Shiny apps
library(shiny)

# Design
library(shinydashboard)
library(shinythemes)
# devtools::install_github("paulc91/shinyauthr")
library(shinyauthr)

# Use Java Script
library(shinyjs)
library(shinyBS)
require(V8)

# For table formatting
library(DT)
library(kableExtra)
library(formattable)

#Piping/operators which promote semantics
library(tidyr)
library(tidyverse)
library(glue)
library(dplyr)
library(magrittr)

#Plotting
library(ggplot2)

#RMarkdown documents for reports 
library(knitr)
library(markdown) # #https://stackoverflow.com/questions/33499651/rmarkdown-in-shiny-application


# install.packages("extrafont")
library(extrafont)
# loadfonts()
# extrafont::font_import()
#windowsFonts()

library(leaflet)

licence0 <- "Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States."


########COLORS#########
NOAAFisheries.Colors<-list(
  
  Oceans = list(
  "Process Blue" = "#0093D0", 
  "Reflex Blue" = "#0055A4", #Nav Bar Hover
  "PMS 541" = "#00467F", # Nav Bar
  "White" = "#FFFFFF"
  ), 
  
  Waves = list(
  "PMS 319" = "#1ECAD3", 
  "PMS 321" = "#008998", 
  "PMS 322" = "#00708", 
  "Gray 10%" = "#E8E8E8"
  ),

  Seagrass = list(
  "PMS 375" = "#93D500", 
  "PMS 362" = "#4C9C2E", 
  "PMS 322" = "#007078", 
  "Gray 20%" = "#D0D0D0"
  ), 
  
  Urchin = list(
  "Custom" = "#7F7FFF", 
  "PMS 2725" = "#625BC4", 
  "PMS 7670" = "#575195",
  "Gray 40%" = "#9A9A9A"
  ), 
  
  Crustacean = list(
    "PMS 151" = "#FF8300", 
    "PMS 717" = "#D65F00", 
    "PMS 7670" = "#575195", 
    "Gray 50%" = "#7B7B7B"
  ), 
  
  Coral = list(
    "Warm Red" = "#FF4438", 
    "PMS 711" = "D02C2F", 
    "PMS 1805" = "#B2292E", 
    "Gray 70%" = "#646464"
  ),
  
  "NOAA Colors" = list(
  
  #Primary Colors
  "REFLEX BLUE" = "#0A4595", 
  "PROCESS BLUE" = "#0099D8", 
  "DARK SLATE GREY" = "#333333", 
  "WHITE" = "#FFFFFF", 
  
  #Secondary Colors
  "DARK GREY" = "#575757", 
  "MEDIUM GREY" = "#666666",
  "LIGHT GREY" = "#ACACAC",
  "FADED BLUE" = "#6B84B4",
  "RICH BLUE GREY" = "#28282A"
  )

)

NOAA.Fonts<-"Proxima Nova"


SameColNames<-function(df.ls) {
  #All column names
  colnames0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    # colnames(df0)<-toupper(colnames(df0))
    df0<-janitor::clean_names(df0)
    df.ls[[i]]<-df0
    colnames0<-c(colnames0, (colnames(df0)))
  }
  colnames0<-sort(unique(colnames0), decreasing = T)
  
  #New df's
  df.ls0<-list()
  df.rbind0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    colnames.out<-colnames0[!(colnames0 %in% colnames(df0))]
    if (length(colnames.out) != 0) {
      for (ii in 1:length(colnames.out)){
        df0[,(ncol(df0)+1)]<-NA
        names(df0)[ncol(df0)]<-colnames.out[ii]
      }
    }
    df0<-df0[,match(table =  colnames(df0), x = colnames0)]
    df.ls0[[i]]<-df0
    names(df.ls0)[i]<-names(df.ls)[i]
    df.rbind0<-rbind.data.frame(df.rbind0, df0)
  }
  return(df.rbind0)
}

