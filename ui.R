

library(shiny)
library(shinydashboard)
source("dash_header.R")

shinyUI(
        dashboardPage(
                skin = "red",
                header <- dash_header(),
                dashboardSidebar(
  
                ),
                dashboardBody()
))
# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))
