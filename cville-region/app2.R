# CF: Work in Progress
# Trying to speed up and fix

# Equity Indicators

# Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyhelper) # for information popups/helper()
library(shinyWidgets) # this might be a holdover from an earlier version of selectors?
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)

# library(sp) # don't think we need this
# library(shinythemes) # this might be for the sidebar/story widgets, or just a holdover from an earlier version
# library(RColorBrewer)
# library(scales)


# Load data, final prep ----
load("www/app_data.Rdata")

# load text for helper(content="")
source("helpers.R")


# extract content from <body> </body> story htmls
#   renderUI(includeHTML()) in server did not render images without removing header info from html; added fix here
#   does this make mores sense as a step in the story creation (done in stories, file resulting htmls copied over)
#   and/or should stories output html files directly to cville-region?
xml2::write_html(rvest::html_node(xml2::read_html("BAed_003.html"), "body"), file = "BAed_003b.html")
xml2::write_html(rvest::html_node(xml2::read_html("alb_homesales_school.html"), "body"), file = "alb_homesales_schoolb.html")


# to ease creation of select/deselect all action
counties <- levels(factor(tract_data_geo$county.nice))


# .....................................................................................

# create ui ----

# the header bar and side menu for story examples
ui <- dashboardPage(
  dashboardHeader(title = "Regional Equity Dashboard Prototype", titleWidth = 400),
  dashboardSidebar(collapsed = TRUE, 
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     menuItem("Example Stories", tabName = "storywidgets", icon = icon("bar-chart-o"),
                              menuSubItem("BA Attainment: Albemarle", tabName = "story1"),
                              menuSubItem("Home Sales: Albemarle", tabName = "story2")),
                     menuItem("Relevant Links", tabName = "linkwidget", icon = icon("external-link"))
                   )
  ), # end header side menu
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              
              # Row 1 panels ----
              fluidRow(
                
                # Sidebar layout
                box(tags$h3("Regional Indicators"),
                    tags$p("Visualizing the greater Charlottesville Region"),
                    
                    # Select localities
                    checkboxGroupInput(
                      inputId = "geo", 
                      label = "Counties", 
                      choices = counties, 
                      selected = counties,
                      inline = TRUE) %>%  # checkboxGroupInput ends, pipe to helper()
                      helper(type = "inline",
                             icon = "info-circle",
                             content = helpers$counties,
                             size = "m"),
                    # add select all option
                    actionButton(inputId = "selectall_geo", 
                                 label = "Select/Unselect All"),  
                    
                    tags$br(),
                    tags$br(),
                    
                    # Select geo level
                    radioButtons(inputId = "df_geo",
                                 label = "Select a Geographic Level:",
                                 choices = c("County", "Census Tract", "Block Group"),
                                 selected = "County",
                                 inline = TRUE) %>% # radioButtons ends, pipe to helper()
                      helper(type = "inline",
                             title = "Geographic Level",
                             icon = "info-circle",
                             content = helpers$geo,
                             size = "m"),
                    
                    # Select category of indicators (optional, to limit number of indicators below)
                    htmlOutput("category") %>% 
                      helper(type = "inline",
                             inputId = "category",
                             icon = "info-circle",
                             content = helpers$category,
                             size = "m"),
                    
                    # Select indicator 1 (for first map, histogram)
                    htmlOutput("indicator") %>% 
                      helper(type = "inline",
                             inputId = "indicator",
                             icon = "info-circle",
                             content = helpers$indicator,
                             size = "m"),
                    
                    # Select indicator 2 (for second map, hisotgram, and scatterplot)
                    #   should we set a default, e.g., to avoid error on map 2 when this is blank?
                    htmlOutput("indicator2") %>% 
                      helper(type = "inline",
                             inputId = "indicator2",
                             icon = "info-circle",
                             content = helpers$indicator2,
                             size = "m"),
                    
                    # Select year (multiple years present for only pop and pop by race vars)
                    #   consider whether this is necessary, or otherways of incorporating change/time
                    htmlOutput("time") %>% 
                      helper(type = "inline",
                             inputId = "time",
                             icon = "info-circle",
                             content = helpers$time,
                             size = "m"),
                    
                    # Select a base map
                    radioButtons(inputId = "map_geo",
                                 label = "Select a Base Map:",
                                 choices = c("Minimal", "Detailed"),
                                 selected = "Minimal",
                                 inline = TRUE) %>% 
                      helper(type = "inline",
                             inputId = "map_geo",
                             icon = "info-circle",
                             content = helpers$map_geo,
                             size = "m"),
                    width=4), # end sidebar layout
                
                # Layout for map 1, map2, data table
                tabBox(tabPanel("Map of Primary Indicator", 
                                textOutput("maptitle"),
                                leafletOutput("map", height=600),
                                textOutput("source")),
                       tabPanel("Map of Secondary Indicator",
                                textOutput("map2title"),
                                leafletOutput("map_compare", height=600),
                                textOutput("source2")),
                       tabPanel("Data Table",
                                textOutput("tbltitle"),
                                tags$div(downloadButton("downloaddf", "Download Data"), style="float: right;"),
                                tags$p("Variables ending in E are estimates; variables ending in M are margins of error."),
                                DTOutput("tbl")),
                       width=8)  # end map layout
                ) # end row 1 panels
              
        
      ), # end second tabitem
      
      tabItem(tabName = "storywidgets"),
      tabItem(tabName = "story1",
              uiOutput("disparity")),  # created in BAed_003.Rmd
      tabItem(tabName = "story2",
              uiOutput("homes")),  # created in alb_homesales_school.Rmd
      tabItem(tabName = "linkwidget",
              uiOutput("links")) # created in equity_links.Rmd
      
    ) # end first tabitems
  ) # end dashboard body
) # end dashboard page


# .....................................................................................

# Consider splitting ui and server into separate scripts?
# create server ----
server <- function(input, output, session) {
  
  # to make helper() info render
  observe_helpers()
  
  # .....................................................................................
  
  # select data sets ----
  # selected geo dataset for map/leaflet
  data_geo <- reactive({
    d1 <- switch(input$df_geo,
                 "County" = county_data_geo,
                 "Census Tract" = tract_data_geo,
                 "Block Group" = blkgrp_data_geo)
    #    d1 <- filter(d1, locality %in% input$geo & major_group2 %in% input$group) # add this back here?
  })
  
  # select non-geo dataset for histogram, scatterplot, table
  df <- reactive({
    d2 <- switch(input$df_geo,
                 "County" = county_data_geo,
                 "Census Tract" = tract_data_geo,
                 "Block Group" = blkgrp_data_geo)
    d2 <- st_drop_geometry(d2)
  })
  
  # select correct pretty table/names for block group, tract, or county level dataset
  #   should this be switch as well to match above?
  prettytab <- reactive({
    pt <- switch(input$df_geo,
                 "Census Tract" = pretty,
                 "County" = pretty2,
                 "Block Group" = pretty3)
  })
  

  # .....................................................................................
  
  # generate selections for UI ----
  
  # Select/Deselect All
  observe({
    if(input$selectall_geo == 0) return(NULL)
    else if (input$selectall_geo%%2 == 0){
      updateCheckboxGroupInput(session, inputId = "geo", "Counties",
                               choices = counties, selected = counties,
                               inline = TRUE)    }else{
                                 updateCheckboxGroupInput(session, "geo", "Counties",
                                                          choices = counties, inline = TRUE)
                               }
  })
  
  # categories of indicators
  output$category <- renderUI({
    selectInput(
      inputId = "category", 
      label = "Limit Indicator by Category",
      choices = levels(factor(prettytab()$group)), 
      selected = levels(factor(prettytab()$group)),
      multiple = T)
  })
  
  # primary indicator
  output$indicator <- renderUI({
    arb <- input$category
    available <- prettytab()[prettytab()$group %in% arb, "goodname"]
    names(available) <- "Indicator 1"
    selectInput(
      inputId = "indicator", 
      label = "Primary Indicator:",
      choices = unique(available),
      selected = unique(available)[1],
      multiple = F)
  })
  
  # secondary indicator
  output$indicator2 <- renderUI({
    arb <- input$category
    available <- prettytab()[prettytab()$group %in% arb, "goodname"]
    names(available) <- "Indicator 2"
    selectInput(
      inputId = "indicator2",
      label = "Secondary Indicator:",
      choices = c("None", unique(available)),
      selected = "None",
      multiple = F)
  })
  
  # multiple years?
  years_avail <- reactive({    
    req(input$indicator)
    df <- df() %>% filter(county.nice %in% input$geo) 
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df <- na.omit(df[,c(col, "year")])
    df$year <- as.numeric(df$year)
    sort(unique(df$year))
  })

  output$time <- renderUI({
    sliderTextInput(inputId = "time", 
                    label = "Select a Year",
                    choices = years_avail(),
                    selected = years_avail()[length(years_avail())],
                    animate=F,
                    grid=T )})
  
  
  # .....................................................................................
  
  # generate title & source information ----
  
  # source captions ----
  # output indicator 1 source, for map caption, plot caption
  output$source <- renderText({
    req(input$indicator)
    paste(prettytab()[prettytab()$goodname==input$indicator, "source"])
  })
  
  # output indicator 1 name, for Source & Definition box
  output$ind1_name <- renderText({
    req(input$indicator)
    paste(input$indicator)
  })
  
  # output indicator 1 description, for Source & Definition box
  output$ind1_abt <- renderText({
    req(input$indicator)
    paste(prettytab()[prettytab()$goodname==input$indicator, "about"]) 
  })
  
  # output indicator 2 source, for map caption, plot caption
  src2 <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      paste(prettytab()[prettytab()$goodname==input$indicator2, "source"])
    }
  })
  
  output$source2 <- renderText({src2()})
  
  # output indicator 2 name, for Source & Definition box
  output$ind2_name <- renderText({
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      paste(input$indicator2)
    }
  })
  
  # output indicator 2 description, for Source & Definition box
  output$ind2_abt <- renderText({
    if (input$indicator2=="None") {
      paste("")
    }
    else {
      req(input$indicator2)
      paste(prettytab()[prettytab()$goodname==input$indicator2, "about"]) }
  })
  
  
  # titles ----
  # map 1 title
  output$maptitle <- renderText({input$indicator})
  
  # map 2 title, if present
  map2.title <- reactive({
    req(input$indicator2)
    if (input$indicator2=="None") {
      paste("You have not selected a second indicator.")
    }
    else {paste(input$indicator2)}
  })
  
  output$map2title <- renderText({map2.title()})
  
  # data table title
  output$tbltitle <- renderText({
    paste("Data by", input$df_geo)
  })
  

  # .....................................................................................
  
  # indicators and points for mapping ----
  # extract indicator 1
  d1 <- reactive({
    req(input$indicator)
    df <- df() %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); nope, change to st_drop_geometry(data_geo())
                             year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    df[,col]
  })
  
  # create indicator 1 palette
  pal <- reactive({
    req(input$indicator)
    df <- df() %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                             year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col]) })
  
  # extract indicator 2
  d2 <- reactive({
    req(input$indicator2)
    df <- df() %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                             year %in% input$time)
    col <- paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])
    df[,col]
  })
  
  # create indicator 2 palette
  pal2 <- reactive({
    req(input$indicator2)
    df <- df() %>% filter(county.nice %in% input$geo &  # change data_geo()@data to data_geo(); change to st_drop_geometry(data_geo())
                             year %in% input$time)
    col2 <- paste(prettytab()[prettytab()$goodname==input$indicator2, "varname"])
    colorNumeric(palette = mycolors,
                 domain = df[,col2])
  })
  

  # other map elements ----
  # create/filter polygons/points
  parks <- reactive({
    parks_sf %>% filter(FIPS %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  schools <- reactive({
    schools_sf %>% filter(county %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  sabselem <- reactive({
    sabselem_sf %>% filter(county %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  mag <- reactive({
    mcd_sf %>% filter(COUNTYFP %in% data_geo()$COUNTYFP[data_geo()$county.nice %in% input$geo])
  })
  
  # choose map tile base
  tile_geo <- reactive({
    tile <- switch(input$map_geo,
                   "Minimal" = "CartoDB.Positron",
                   "Detailed" = "OpenStreetMap.Mapnik")
  })
  

  # .....................................................................................
  
  # output maps ----
  # map 1
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(tile_geo()) %>% 
      addPolygons(data = subset(data_geo(), county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal()(d1()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator, ": ",d1(),  "<br>",
                                 data_geo()$NAME[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks(), group="Parks", 
                  color = "green",
                  popup = paste(parks()$NAME)) %>% 
      addCircles(data =  schools(), group="Schools", 
                 color = "blue",
                 popup = paste(schools()$NAME)) %>% 
      addPolygons(data = sabselem(), group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = paste(sabselem()$schnam),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>% 
      addPolygons(data = mag(), group="Magesterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = paste(mag()$NAMELSAD),
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magesterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magesterial Districts") %>% 
      addLegend(pal = pal(),
                values = as.numeric(d1()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator)  
  })
  
  
  # map 2 
  output$map_compare <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(tile_geo()) %>% 
      addPolygons(data = subset(data_geo(), county.nice %in% input$geo & year %in% input$time),
                  fillColor = ~pal2()(d2()),
                  fillOpacity = 0.5,
                  color = "white",
                  weight = 2,
                  smoothFactor = 0.2,
                  popup = paste0(input$indicator2, ": ",d2(),  "<br>",
                                 data_geo()$NAME[data_geo()$county.nice %in% input$geo & data_geo()$year %in% input$time], "<br>"
                  ),
                  highlight = highlightOptions(
                    weight = 5,
                    fillOpacity = 0.7,
                    bringToFront = F)) %>% 
      addPolygons(data = subset(counties_geo, NAMELSAD %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 3) %>% 
      addPolygons(data =  parks(), group="Parks", 
                  popup = paste(parks()$NAME)) %>% 
      addCircles(data =  schools(), group="Schools", 
                 popup = paste(schools()$NAME)) %>% 
      addPolygons(data = sabselem(), group="Elem School Zone",
                  color = "blue", fill = FALSE, weight = 2,
                  popup = paste(sabselem()$schnam),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = TRUE)) %>% 
      addPolygons(data = mag(), group="Magesterial Districts",
                  color = "purple", fill = FALSE, weight = 2,
                  popup = paste(mag()$NAMELSAD),
                  highlight = highlightOptions(weight = 3,
                                               color = "purple",
                                               bringToFront = TRUE)) %>% 
      addLayersControl(
        overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magesterial Districts"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "bottomright"
      ) %>% 
      hideGroup("Parks") %>% 
      hideGroup("Schools") %>% 
      hideGroup("Elem School Zone") %>% 
      hideGroup("Magesterial Districts") %>% 
      addLegend(pal = pal2(),
                values = as.numeric(d2()),
                position = "topright",
                opacity = 0.25,
                title = input$indicator2)  
  })
  
  
  # .....................................................................................
  
  # output data table ----
  output$tbl <-  renderDT({
    datatable(subset(df(), county.nice %in% input$geo & year %in% input$time),
              options = list(scrollX = TRUE))
  })
  
  # download csv of data
  output$downloaddf <- downloadHandler(
    filename = function() {
      paste("cville-region-", input$df_geo, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df(), file, row.names = FALSE)
    }
  )
  
  
  
  
  # .....................................................................................
  
  # output sidebar stories ----
   output$tabs <- renderText("Story Examples")
   output$disparity <- renderUI(includeHTML("BAed_003b.html"))
   output$homes <- renderUI(includeHTML("alb_homesales_schoolb.html"))
   output$links <- renderUI(includeHTML("equity_links.html"))

}


# run the application 
shinyApp(ui = ui, server = server)
