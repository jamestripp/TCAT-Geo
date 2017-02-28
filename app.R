#######################################################################
# TCAT Geo Tool -- Shiny app to plot TCAT data on a map
# 
# Author: James, Tripp
#
# Purpose: Academic tool to help CIM students visualise tweet data
#
# Usage:
#     Click on Run App within RStudio.
#
######################################################################

options(shiny.maxRequestSize = 600 * 1024 ^ 2)

# Install libraries if unavailable
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("data.table")) install.packages("data.table")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

# options used for drop down menu
# you can change the column names to match the columns in your data
mapProviders <- c('OpenStreetMap.BlackAndWhite', 'OpenStreetMap.Mapnik', 'Stamen.Watercolor', 'Stamen.Toner', 'Esri.WorldImagery')
textColumns <- c('from_user_realname', 'text', 'location', 'from_user_name', 'from_user_description', 'from_user_url', 'from_user_timezone', 'from_user_lang')
numColumns <- c('retweet_count', 'favorite_count', 'from_user_verified', 'from_user_tweetcount', 'from_user_followercount', 'from_user_friendcount', 'from_user_favourites_count', 'from_user_listed')


# Define user interface
ui <- navbarPage("TCAT Geo Tool", id = "nav",

# Instruction panel
                 tabPanel('Start',
                          tags$h1('Instructions'),
                          tags$p('In this app you can load tweets downloaded from TCAT in .csv format. Then you can visualise the location of the tweets.'),
                          tags$p('To start, click on the browse button below and select the .csv file containing the tweets'),
# Data load interface
                          fileInput('file1', '',
                                    accept = c('text/csv',
                                             'text/comma-separated-values,text/plain',
                                             '.csv')),
                          tags$p('Once the upload is complete you can view the tweets in a map and the raw data by clicking on the "Map" and "Data" tabs at the top of the page.'),
                          tags$p('Have fun!'),
                          tags$p('James Tripp, CIM, University of Warwick, 2017.'),
# The styles.css file is taken from the superzip example
                          tags$strong('Note:'),
                          tags$p('This application includes code from:'),
                          tags$a(href = 'https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example', 'https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example')
                          ),


                 tabPanel("Data",
# display the data in a data table format (see the server logic below)
                          dataTableOutput("contents")
                 ),

                 tabPanel("Map",
                          div(class = "outer",
# insert the CSS styling - the css styling is taken from the Shiny SuperZip example             
                              tags$head(
                                tags$style(HTML("
                                   input[type='number'] {
                                    max-width: 80%;
                                   }
                                   
                                   div.outer {
                                   position: fixed;
                                   top: 41px;
                                   left: 0;
                                   right: 0;
                                   bottom: 0;
                                   overflow: hidden;
                                   padding: 0;
                                   }
                                   
                                   /* Customize fonts */
                                   body, label, input, button, select { 
                                   font-family: 'Helvetica Neue', Helvetica;
                                   font-weight: 200;
                                   }
                                   h1, h2, h3, h4 { font-weight: 400; }
                                   
                                   #controls {
                                   /* Appearance */
                                   background-color: white;
                                   padding: 0 20px 20px 20px;
                                   cursor: move;
                                   /* Fade out while not hovering */
                                   opacity: 0.65;
                                   zoom: 0.9;
                                   transition: opacity 500ms 1s;
                                   }

                                    #controls:hover {
                                    /* Fade in while hovering */
                                    opacity: 0.95;
                                    transition-delay: 0;
                                    }
                                    
                                    /* Position and style citation */
                                    #cite {
                                    position: absolute;
                                    bottom: 10px;
                                    left: 10px;
                                    font-size: 12px;
                                    }
                                    
                                    /* If not using map tiles, show a white background */
                                    .leaflet-container {
                                    background-color: white !important;
                                    }
                                    
                                    "))
                                ),

                              leafletOutput("map", width = "100%", height = "100%"),

# define our movable menu
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            tags$strong('The map background :'),
# drop down options for the colour varianle
                                            selectInput(inputId = "mapVariable",
                                                        label = "Map type",
                                                        choices = mapProviders
                                            ),
                                            tags$strong('Text displayed when clicking on marker.'),
# define drop down box for the text displayed on the marker
                                            selectInput(inputId = "markerText",
                                                        label = "Popup text column",
                                                        choices = textColumns
                                            ),
                                            tags$strong('Marker size options:'),
                                            selectInput(inputId = "sizeVariable",
                                                        label = "Size column",
                                                        choices = numColumns
                                            ),
# defines a multiplier for the radius of the circle markers
                                            numericInput(inputId = 'radiusMultiplier', step = 0.01,
                                                         label = 'Radius multiplier:',
                                                         min = 0,
                                                         max = 1000,
                                                         value = 0
                                            ),
# drop down options for the colour varianle
                                            selectInput(inputId = "colourVariable",
                                                        label = "Colour column",
                                                        choices = numColumns
                                            ),
# this gives us the range of possible colour choices
                                            selectInput("colors", "Color Scheme",
                                                        rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                            ),
# we can only add a legend for colour. This option add this.
                                            checkboxInput("legend", "Click to show colour legend.", TRUE)
                              )
                          )
                 )
)

# Define server logic
server <- function(input, output) {

    # load the csv content whenever we upload a new file
    d <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        # For some reason, not specifying the fileEncoding works on Windows but not Mac
        if (Sys.info()['sysname'] == 'Windows') {
            read.csv(inFile$datapath, stringsAsFactors = FALSE)
        } else {
            read.csv(inFile$datapath, stringsAsFactors = FALSE, fileEncoding = "latin1")
        }
    })

    # These update when ui elements change
    formulaText <- reactive({
        paste('~', input$markerText)
    })

    maps <- reactive({
        input$mapVariable
    })

    radiusNum <- reactive({
        input$sizeVariable
    })

    colourNum <- reactive({
        input$colourVariable
    })

    radiusMul <- reactive({
        input$radiusMultiplier
    })

    #render csv data as a data table
    output$contents <- renderDataTable({
        d()
    })

    # creates our map
    output$map <- renderLeaflet({
        # copy data to temporary variable
        tmp.d <- d()

        # remove rows with no geo reference data
        tmp.d <- tmp.d[!is.na(tmp.d$lat),]

        # multiply radius varianle by multiplier
        currentRadius <- reactive({
            tmp.d[, radiusNum()] * radiusMul()
        })

        # create new column with radius
        tmp.d$radius <- currentRadius()

        # set colour pallette according to ui selection
        colourPal <- reactive({
            colorNumeric(palette = input$colors, domain = tmp.d[, colourNum()])
        })

        # get current colour pallette
        pal <- colourPal()

        # create map
        m <- leaflet(tmp.d) %>%
      addProviderTiles(maps(),
                       options = providerTileOptions(noWrap = TRUE)
      )
        if (max(tmp.d$radius) == 0) {
            # do not change radius if radius variable contains 0
            m <- m %>% addCircleMarkers(stroke = TRUE, color = 'black', ~ lng, fillColor = ~pal(tmp.d[, colourNum()]), ~ lat, popup = as.formula(formulaText()))
        } else {
            # use radius varianble
            m <- m %>% addCircleMarkers(stroke = TRUE, color = 'black', ~ lng, fillColor = ~pal(tmp.d[, colourNum()]), ~ lat, radius = ~radius, popup = as.formula(formulaText()))
        }

        if (length(unique(tmp.d[, colourNum()])) == 1) {

        } else {
            # if there are multiple value of the colour variable
            if (input$legend) {
                # if legend button has been clicked
                m <- m %>% addLegend(pal = pal, values = tmp.d[, colourNum()], title = colourNum())
            }
        }

        # show map
        m

    })
}

# Run the application 
shinyApp(ui = ui, server = server)