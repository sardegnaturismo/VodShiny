

library(shiny)
library(shinydashboard)
source("dash_header.R")
source("build_sidebar.R")
source("build_items.R")

shinyUI(
       
        dashboardPage(
                skin = "red",
                header <- dash_header(),
                sidebar <- build_sidebar(),
                dashboardBody(
                  items <- build_items(),
                  tags$head(tags$script(src="if-resize.js"))
                )
))

