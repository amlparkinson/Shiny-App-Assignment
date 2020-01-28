# Add packages ------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(raster)
library(rgdal)

# add data ----------------------------------------------------------------

thomas <- shapefile(here("thomas_scar", "Thomas_Fire_Boundary_utm.shp"))

# create initial user interface ---------------------------------------------------

ui <- fluidPage(
  mainPanel("Fire perimeter here",
            map)
)
server <- function(input, output) {}

shinyApp(ui = ui, server = server)



