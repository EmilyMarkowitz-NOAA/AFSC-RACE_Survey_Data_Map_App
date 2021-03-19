### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.surveymap <- function() {
  tabItem(
    tabName = "surveymap",
    fluidRow(
      HTML("<html lang='en'>"), #Always have this as your first line
      
      div(class="outer",
          
          tags$head(
            # Include our custom CSS
            includeCSS("styles.css"),
            includeScript("gomap.js")
          ),
          
          # If not using custom CSS, set height of leafletOutput to a number instead of percent
          leafletOutput("survey_leaflet", width="100%", height="100%"),
          
          # Shiny versions prior to 0.11 should use class = "modal" instead.
          absolutePanel(id = "controls", 
                        class = "panel panel-default", 
                        fixed = TRUE,
                        draggable = TRUE, top = 60, 
                        left = "auto", right = 20, 
                        bottom = "auto",
                        width = 330, height = "auto",
                        
                        h2(" "),
                        
                        # sliderInput(inputId = "slider", 
                        #             label = "values",
                        #             min = 0,
                        #             max = 100,
                        #             value = 0,
                        #             step = 1),
                        selectInput("year", "Year:", 
                                    choices = sort(unique(df0$year)), 
                                    selected = max(df0$year)),
                        selectInput("spp", "Species:", 
                                    choices = sort(unique(df0$common_name)), 
                                    selected = "Pacific halibut"),
                        selectInput("survey", "Survey:", 
                                    choices = c(sort(unique(df0$survey))), 
                                    selected = c("EBS", "NBS"), 
                                    multiple = TRUE),
                        selectInput("cpue_unit", "CPUE Unit:", 
                                    choices = c("kg of fish/ha", 
                                                "number of fish/ha", 
                                                "None"), 
                                    selected = "kg of fish/ha"),
                        checkboxInput("stat_points", "Station Points", value = TRUE),
                        checkboxInput("cpue_points", "CPUE Points (only if CPUE Unit is not = None", value = TRUE),
                        checkboxInput("cpue_idw", "CPUE IDW", value = FALSE),
                        checkboxInput("cpue_bt", "Bottom Temperature IDW", value = FALSE),
                        checkboxInput("stratum", "Stratum", value = TRUE), 
                        
                        downloadButton(outputId = "dl_map", label = "Download Map (PNG)")
                        
                        # leafletOutput("survey_leaflet")                                            conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                        # Only prompt for threshold when coloring or sizing by superzip
                        #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                        # ),
                        
                        # plotOutput("histCentile", height = 200),
                        # plotOutput("scatterCollegeIncome", height = 250)
          ),
          
          tags$div(id="cite",
                   paste0('Data last updated XXXX and this app was last updated ', format(Sys.Date(), format='%B %d %Y'),'.')
          )
      )
    )
      

  )
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

