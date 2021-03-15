# Clay's version of equity dashboard

library(shiny)
library(shinydashboard)
library(shinyhelper)
library(shinyWidgets) # for sliderTextInput()
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(DT)

# plotly version '4.9.2.1' causes dplyr to issue warning:
# Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
# Please use `arrange()` instead.

# Load Data ---------------------------------------------------------------

load("www/app_data2.Rdata")

st_crs(all_data) <- 4326
st_crs(counties_geo) <- 4326


# Define UI ---------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Regional Equity Dashboard Prototype", titleWidth = 400),
    dashboardSidebar(collapsed = TRUE, 
                     sidebarMenu(
                         menuItem("Dashboard", tabName = "dashboard", 
                                  icon = icon("dashboard")),
                         menuItem("Example Stories", tabName = "storywidgets", 
                                  icon = icon("bar-chart-o"),
                                  menuSubItem("BA Attainment: Albemarle", tabName = "story1"),
                                  menuSubItem("Home Sales: Albemarle", tabName = "story2")),
                         menuItem("Relevant Links", tabName = "linkwidget", 
                                  icon = icon("external-link"))
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
  
            # pick counties
            checkboxGroupInput(
                inputId = "geo", 
                label = "Counties", 
                choices = counties, 
                selected = counties,
                inline = TRUE) %>%
              helper(type = "inline",
                     icon = "info-circle",
                     content = helpers$counties,
                     size = "m"),
            
            # add select all option
            actionButton(inputId = "selectall_geo", 
                         label = "Select/Unselect All"),

            tags$br(),
            
            # pick geographic level
            radioButtons(inputId = "geo_df",
                         label = "Select a Geographic Level:", 
                         choices = c("County", "Census Tract", "Block Group"),
                         selected = "County") %>% 
              helper(type = "inline",
                     title = "Geographic Level",
                     icon = "info-circle",
                     content = helpers$geo,
                     size = "m"),

            # pick indicator 1
            selectInput("indicator1",
                        "Primary Indicator:",
                        choices = ind_choices_county,
                        selected = ind_choices_county$People['Estimated Population']) %>% 
              helper(type = "inline",
                     icon = "info-circle",
                     content = helpers$indicator,
                     size = "m"),
            
            # pick indicator 2
            selectInput("indicator2",
                        "Secondary Indicator:",
                        choices = c("None", ind_choices_county),
                        selected = "None") %>% 
              helper(type = "inline",
                     icon = "info-circle",
                     content = helpers$indicator2,
                     size = "m"),
            
            # pick year (only available for vars in year_ind)
            sliderTextInput("time",
                        "Select a Year:",
                        choices = years,
                        selected = max(years),
                        grid = TRUE) %>% 
              helper(type = "inline",
                     icon = "info-circle",
                     content = helpers$time,
                     size = "m"),
            
            # Select a base map
            radioButtons(inputId = "map_geo",
                         label = "Select a Base Map:",
                         choices = c("Minimal" = "CartoDB.Positron",
                                     "Detailed" = "OpenStreetMap.Mapnik"),
                         inline = TRUE) %>% 
              helper(type = "inline",
                     inputId = "map_geo",
                     icon = "info-circle",
                     content = helpers$map_geo,
                     size = "m"),
            
            width=4), # end sidebar layout

        # Layout for map 1, map2, data table
        tabBox(id = "tabs", 
               tabPanel("Map of Primary Indicator",
                        value = "tab1",
                        textOutput("maptitle"),
                        leafletOutput("map", height=600),
                        textOutput("source")),
               tabPanel("Map of Secondary Indicator",
                        value = "tab2",
                        textOutput("maptitle2"),
                        leafletOutput("map2", height=600),
                        textOutput("source2")),
               tabPanel("Data Table",
                        textOutput("tbltitle"),
                        tags$div(downloadButton("downloaddf", "Download Data"), 
                                 style="float: right;"),
                        tags$p("Variables ending in E are estimates;
                               variables ending in M are margins of error."),
                        DTOutput("tbl")),
               width=8)  # end map layout
            ), # end row 1 panels
        
        fluidRow(
          # Provide definitions, source information
          box(title = "Indicator Source & Definition",
              strong(textOutput("ind1_name")), 
              textOutput("ind1_abt"), 
              tags$br(),
              strong(textOutput("ind2_name")), 
              textOutput("ind2_abt"),
              width=4),
          
          # Histogram, scatterplot, and aggregation by race tabs
          tabBox(tabPanel("Distribution",
                          textOutput("histtitle") %>% 
                            helper(type = "inline",
                                   title = "Distribution",
                                   icon = "question-circle",
                                   content = helpers$distribution,
                                   size = "m"),
                          plotlyOutput("hist"),
                          textOutput("source_b")), 
                 tabPanel("Correlation",
                          textOutput("comptitle") %>% 
                            helper(type = "inline",
                                   title = "Distribution",
                                   icon = "question-circle",
                                   content = helpers$correlation,
                                   size = "m"),
                          plotlyOutput("compare"),
                          textOutput("source_c")), 
                 tabPanel("By Race", 
                          textOutput("racetitle") %>% 
                            helper(type = "inline",
                                   title = "By Race",
                                   icon = "question-circle",
                                   content = helpers$race,
                                   size = "m"),
                          plotlyOutput("byrace")),
                 width=8)
        ),    # end row 2 panels
        # Row 3 panels ----
        #   attributions (consider moving these to an about tab?)
        fluidRow(
          # https://stackoverflow.com/questions/30534674/displaying-true-when-shiny-files-are-split-into-different-folders/30538596#30538596
          box(source('www/tags_equity.R', local = TRUE)$value),
          box(source('www/tags_commpas.R', local = TRUE)$value)
          
        ) # end row 3 panels
            ), # end second tabitem
        
        tabItem(tabName = "storywidgets"),
        tabItem(tabName = "story1",
                uiOutput("disparity")),  # created in BAed_003.Rmd
        tabItem(tabName = "story2",
                uiOutput("homes")),  # created in alb_homesales_school.Rmd
        tabItem(tabName = "linkwidget",
                uiOutput("links")) # created in equity_links.Rmd)
        )
        )
)

# Define server logic -----------------------------------------------------

server <- function(input, output, session) {

  # to make helper() info render
  observe_helpers()
  
    # county selections
    observe({
        if (input$selectall_geo == 0) return(NULL)
        else if (input$selectall_geo %% 2 == 0){
            updateCheckboxGroupInput(session, inputId = "geo", "Counties",
                                     choices = counties, selected = counties,
                                     inline = TRUE)
            } else { 
                updateCheckboxGroupInput(session, "geo", "Counties",
                                         choices = counties, inline = TRUE)
                }
    })
    
    # geography selections - update selection of indicators based on geo level
    observeEvent(input$geo_df, {
        if (input$geo_df == "Block Group"){
            updateSelectInput(session, "indicator1", choices = ind_choices_bg, 
                              selected = ind_choices_bg$People['Estimated Population'])
            updateSelectInput(session, "indicator2", choices = c("None", ind_choices_bg),
                              selected = "None")
        } else if (input$geo_df == "Census Tract"){
            updateSelectInput(session, "indicator1", choices = ind_choices_ct,
                              selected = ind_choices_ct$People['Estimated Population'])
            updateSelectInput(session, "indicator2", choices = c("None", ind_choices_ct),
                              selected = "None")
        } else {
            updateSelectInput(session, "indicator1", choices = ind_choices_county,
                              selected = ind_choices_county$People['Estimated Population'])
            updateSelectInput(session, "indicator2", choices = c("None", ind_choices_county),
                              selected = "None")
        }
      })
    
    # get map data
    md <- reactive({
        all_data %>% filter(county.nice %in% input$geo &
                                GEO_LEVEL == input$geo_df &
                                year == input$time) 
        })
    
    
    ################
    #### BASE MAP
    ################
    
    output$map <- output$map2 <- renderLeaflet({
        
        # filter for "Parks", "Schools", "Elem School Zone", "Magisterial Districts"
        f <- md()[["COUNTYFP"]]
        
        counties_geo %>%
          # sf::st_transform(4326) %>%
          leaflet() %>%
          addProviderTiles(input$map_geo) %>%
          addPolygons(color = "grey",
                        fill = FALSE,
                        weight = 3) %>% 
          addPolygons(data =  filter(parks_sf, FIPS %in% f), 
                        group="Parks", 
                        popup = ~NAME) %>% 
          addCircles(data =  filter(schools_sf, county %in% f), 
                       group="Schools", 
                       popup = ~NAME) %>% 
          addPolygons(data = filter(sabselem_sf, county %in% f), 
                        group="Elem School Zone",
                        color = "blue", fill = FALSE, weight = 2,
                        popup = ~schnam,
                        highlight = highlightOptions(weight = 3,
                                                     color = "blue",
                                                     bringToFront = TRUE)) %>% 
          addPolygons(data = filter(mcd_sf, COUNTYFP %in% f), 
                        group="Magisterial Districts",
                        color = "purple", fill = FALSE, weight = 2,
                        popup = ~NAMELSAD,
                        highlight = highlightOptions(weight = 3,
                                                     color = "purple",
                                                     bringToFront = TRUE)) %>% 
          addLayersControl(
                overlayGroups = c("Parks", "Schools", "Elem School Zone", "Magisterial Districts"),
                options = layersControlOptions(collapsed = FALSE), 
                position = "bottomright"
            ) %>% 
          hideGroup("Parks") %>% 
          hideGroup("Schools") %>% 
          hideGroup("Elem School Zone") %>% 
          hideGroup("Magisterial Districts") 

        
    })

    ################
    #### BEGIN MAP 1
    ################
    
    observe({
        if(input$tabs == "tab1"){

        # vector of values
            ind1 <- md() %>% 
                filter(!is.na(.data[[input$indicator1]])) %>% 
                pull(input$indicator1)
        
        if (all(is.na(ind1))){
            showModal(modalDialog(
                title = "Data not available",
                "Data not available for the current Geographic Level or selected Year." ))
        } else {
            leafletProxy("map", data = md()
                         # data = sf::st_transform(md(), 4326)
                         ) %>%
            clearControls() %>%
            addProviderTiles(input$map_geo) %>%
            addPolygons(fillColor = colorNumeric(mycolors, domain = ind1)(ind1),
                            fillOpacity = 0.5,
                            color = "#969997",
                            weight = 2,
                            smoothFactor = 0.2,
                            popup = paste0(attr(ind1, "goodname"), ": ",
                                           ind1, "<br>",
                                           md()[["NAME"]], "<br>")) %>%
                addLegend(pal = colorNumeric(mycolors, domain = ind1),
                          values = ind1,
                          position = "topright",
                          opacity = 0.25,
                          title = attr(ind1, "goodname"))
          }
        }
    })
    
    output$maptitle <- renderText({paste0(attr(md()[[input$indicator1]], "goodname"),
                                          ", ", input$time) })
    output$source <- renderText({attr(md()[[input$indicator1]], "source")})
    

    ################
    #### BEGIN MAP 2
    ################
    
    observe({
      
      if (input$tabs == "tab2"){
        
      # redraw a basic map if None selected again
      if (input$indicator2 == "None"){
        leafletProxy("map2", data = md()
                     # data = sf::st_transform(md(), 4326)
                     ) %>%
          clearControls() %>% 
          clearShapes() %>%
          addPolygons(color = "grey",
                      fill = FALSE,
                      weight = 3)
        
        } else {  
          
        # vector of values
        ind2 <- md() %>% 
            filter(!is.na(.data[[input$indicator2]])) %>% 
            pull(input$indicator2)
        
        if (all(is.na(ind2))){
            showModal(modalDialog(
                title = "Data not available",
                "Data not available for the current Geographic Level or selected Year." ))
          } else {
            leafletProxy("map2", data = md()
                         # data = sf::st_transform(md(), 4326)
                         ) %>%
                clearControls() %>% 
                addProviderTiles(input$map_geo) %>% 
                addPolygons(fillColor = colorNumeric(mycolors, domain = ind2)(ind2),
                            fillOpacity = 0.5,
                            color = "#969997",
                            weight = 2,
                            smoothFactor = 0.2,
                            popup = paste0(attr(ind2, "goodname"), ": ",
                                           ind2, "<br>",
                                           md()[["NAME"]], "<br>")) %>%
              addLegend(pal = colorNumeric(mycolors, domain = ind2),
                          values = ind2,
                          position = "topright",
                          opacity = 0.25,
                          title = attr(ind2, "goodname"))
          }
        } 
      }
    })
    
    output$maptitle2 <- renderText({
      if (input$indicator2 != "None") paste0(attr(md()[[input$indicator2]], "goodname"),
                                          ", ", input$time) })
    output$source2 <- renderText({
      if (input$indicator2 != "None") attr(md()[[input$indicator2]], "source")})
    
    
    ########################
    #### BEGIN DATA DOWNLOAD
    ########################
    
    # output data table ----
    output$tbl <-  renderDT({
      datatable(st_drop_geometry(md()),
                options = list(scrollX = TRUE))
    })
    
    # data table title
    output$tbltitle <- renderText({
      paste("Data by", input$geo_df)
    })
    
    # download csv of data
    output$downloaddf <- downloadHandler(
      filename = function() {
        paste0("cville-region-", input$geo_df, ".csv")
      },
      content = function(file) {
        write.csv(md(), file, row.names = FALSE)
        })

    ###############################
    # BEGIN Source & Definition box
    ###############################
    
    # output indicator 1 name, for Source & Definition box
    output$ind1_name <- renderText({
      attr(md()[[input$indicator1]], "goodname")
    })
    
    # output indicator 1 description, for Source & Definition box
    output$ind1_abt <- renderText({
      attr(md()[[input$indicator1]], "about") 
    })
    
    # output indicator 2 name, for Source & Definition box
    output$ind2_name <- renderText({
      req(input$indicator2)
      attr(md()[[input$indicator2]], "goodname")
    })
    
    # output indicator 2 description, for Source & Definition box
    output$ind2_abt <- renderText({
      req(input$indicator2)
      attr(md()[[input$indicator2]], "about") 
    })
    
    
    ###############################
    # BEGIN Histogram(s)
    ###############################

    
    output$histtitle <- renderText({
      if (input$indicator2=="None") {
        paste("Histogram of", attr(md()[[input$indicator1]], "goodname"), "by", input$df_geo)
      } else {
        paste("Histograms of", attr(md()[[input$indicator1]], "goodname"), " and ",
              attr(md()[[input$indicator2]], "goodname"), "by Census Tract")
        }
    })
    
    output$hist <- renderPlotly({ 
      d <- st_drop_geometry(md())
      if (!input$indicator2=="None") {
        p1 <- plot_ly(data = d,  
                      x = ~get(input$indicator1),
                      type="histogram", 
                      marker = list(color = '#2c7fb8')) %>%
          layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname")))
        p2 <- plot_ly(data = d,  
                      x = ~get(input$indicator2),
                      type="histogram", 
                      marker = list(color = '#41b6c4')) %>%
          layout(xaxis=list(title=attr(d[[input$indicator2]], "goodname")))
        
        subplot(p1, p2, titleX = T, titleY = T) %>%
          layout(showlegend = FALSE)
      } else {
        plot_ly(data = d,  
                x = ~get(input$indicator1),
                type="histogram", marker = list(color = '#253494')) %>%
          layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname")))
      }
    })
    
    # histogram caption: source 1 or source 1 & 2 
    output$source_b <- renderText({
      if (input$indicator2=="None") {
        attr(md()[[input$indicator1]], "source")
      } else {
        paste0(
          attr(md()[[input$indicator1]], "goodname"), ": ", 
          attr(md()[[input$indicator1]], "source"), " ",
          attr(md()[[input$indicator2]], "goodname"), ": ", 
          attr(md()[[input$indicator2]], "source")
        )
      }
    })
    
    ###############################
    # BEGIN Scatterplot
    ###############################  
    

    output$comptitle <- renderText({
      if (input$indicator2=="None") {
        paste("You have not selected a second indicator. To compare, select a second indicator from the control panel.")
      } else { 
        paste(attr(md()[[input$indicator1]], "goodname"), " vs. ", 
              attr(md()[[input$indicator2]], "goodname"))
        }
    })
    
    output$compare <- renderPlotly({ # add loess line to this?
      d <- st_drop_geometry(md())
      if (!input$indicator2=="None") {
        plot_ly(data=d,  
                x = ~get(input$indicator1),
                y=  ~get(input$indicator2),
                color = ~county.nice,
                type = "scatter", 
                mode = "markers", 
                # Set3 has 12 values, which matches the 12 cities/counties
                colors = "Set3", 
                text = paste0(d$county.nice, "<br>",
                              attr(d[[input$indicator1]], "goodname"), ": ", 
                              d[[input$indicator1]], "<br>",
                              attr(d[[input$indicator2]], "goodname"), ": ", 
                              d[[input$indicator2]]
                ), 
                hoverinfo='text') %>% 
          layout(xaxis=list(title=attr(d[[input$indicator1]], "goodname")),
                 yaxis=list(title=attr(d[[input$indicator2]], "goodname")))
      }
    })
    
    # scatterplot source caption, if present
    output$source_c <- renderText({
      if (input$indicator2=="None") {
        paste("")
      } else {
        paste(
          attr(md()[[input$indicator1]], "goodname"), ": ", 
          attr(md()[[input$indicator1]], "source"), " ",
          attr(md()[[input$indicator2]], "goodname"), ": ", 
          attr(md()[[input$indicator2]], "source")
        )
      }
    })
    
    
    ###############################
    # BEGIN By Race plot
    ###############################  
    
    # by race title, if present
    output$racetitle <- renderText({
      if (!input$indicator1 %in% race_vars) {
        paste("Breakouts by race are not yet available for the primary indicator you selected.")
      } else {
        paste(attr(md()[[input$indicator1]], "goodname"), "by Race by Locality")}
    })
    
    # output by race visual ----
    # part of the patched together aggregation by race
    output$byrace <- renderPlotly({
      if (input$indicator1 %in% race_vars) {
        ggplotly(
          ggplot(subset(race_comp, NAME %in% input$geo & var == input$indicator1),
                 aes(x=race, y=value, fill=race)) + 
            stat_summary(fun="sum", geom="bar", 
                         aes(text = paste(NAME,"<br>", race, 
                                         input$indicator,
                                         prettyNum(value, big.mark=",",scientific=F)))) + 
            scale_fill_manual(values=brewer.pal(6, "YlGnBu")[2:6]) +
            facet_wrap(~NAME, scales="free_y") +
            labs(x="", y="") +
            theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0)),
          tooltip="text") %>%
          layout(showlegend = FALSE)
      }
    })
    
    ###############################
    # BEGIN sidebar stories
    ###############################  
    
    output$tabs <- renderText("Story Examples")
    output$disparity <- renderUI(includeHTML("BAed_003b.html"))
    output$homes <- renderUI(includeHTML("alb_homesales_schoolb.html"))
    output$links <- renderUI(includeHTML("equity_links.html"))
}

# Run the application 
shinyApp(ui = ui, server = server)

