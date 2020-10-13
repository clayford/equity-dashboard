# try combining all data sets
names(pretty)
length(pretty$varname)
length(names(tract_data_geo))

# add pretty labels, sources and about to data frames
j <- match(pretty$varname, names(tract_data_geo))

for(i in seq_along(j)){
  attr(tract_data_geo[[j[i]]], which = "goodname") <- pretty$goodname[i]
  attr(tract_data_geo[[j[i]]], which = "source") <- pretty$source[i]
  attr(tract_data_geo[[j[i]]], which = "about") <- pretty$about[i]
}

j <- match(pretty2$varname, names(county_data_geo))
j <- j[!is.na(j)]
for(i in seq_along(j)){
  attr(county_data_geo[[j[i]]], which = "goodname") <- pretty2$goodname[i]
  attr(county_data_geo[[j[i]]], which = "source") <- pretty2$source[i]
  attr(county_data_geo[[j[i]]], which = "about") <- pretty2$about[i]
}

j <- match(pretty3$varname, names(blkgrp_data_geo))
j <- j[!is.na(j)]
for(i in seq_along(j)){
  attr(blkgrp_data_geo[[j[i]]], which = "goodname") <- pretty3$goodname[i]
  attr(blkgrp_data_geo[[j[i]]], which = "source") <- pretty3$source[i]
  attr(blkgrp_data_geo[[j[i]]], which = "about") <- pretty3$about[i]
}

# Combine data
all_data <- rbind()



library(dplyr)
all_data <- bind_rows("County" = county_data_geo, 
                      "Block Group" = blkgrp_data_geo, 
                      "Census Tract" = tract_data_geo, 
                      .id = "Geographic Level")


leaflet(subset(all_data, `Geographic Level` == "County" & year == "2018")) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = mycolors,
                fillOpacity = 0.5,
                color = "white",
                weight = 2,
                smoothFactor = 0.2) %>% 
    addPolygons(data = counties_geo,
                color = "grey",
                fill = FALSE,
                weight = 3) %>% 
    addLegend(pal = colorNumeric("Blues", domain = all_data$totalpopE),
              values = ~totalpopE,
              position = "topright",
              opacity = 0.25,
              title = "Population")  



# labels ------------------------------------------------------------------

# Trying to figure out a way to get away from the "pretty" data frames and
# include labels with the demographic data frame.


data("iris")
str(iris)
labels(iris)
library(Hmisc)
label(iris$Sepal.Length) <- "Sepal Length (cm)"
label(iris$Sepal.Width) <- "Sepal Width (cm)"
label(iris$Petal.Length) <- "Petal Length (cm)"
label(iris$Petal.Width) <- "Petal Width (cm)"
label(iris$Species) <- "Iris Species"
str(iris)

hist(iris$Sepal.Length, main = attr(iris$Sepal.Length, "label"))

# selectInput("variable", "Variable:",
#             c("Cylinders" = "cyl",
#               "Transmission" = "am",
#               "Gears" = "gear")

pairs(subset(iris, Species == "setosa"))


l <- sapply(iris[1:4], function(x)attr(x, which = "label"))
choices <- names(iris)[1:4]
names(choices) <- l


library(shiny)
ui <- fluidPage(
  selectInput(inputId = "var", label = "variable", choices = choices),
  plotOutput(outputId = "hist")
)

server <- function(input, output, session) {
  output$hist <- renderPlot(hist(iris[[input$var]]))
}
shinyApp(ui, server)
