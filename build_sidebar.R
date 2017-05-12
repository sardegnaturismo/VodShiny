build_sidebar <- function(){
  
  dashboardSidebar(
    sidebarMenu(id="items",
                menuItem("home", tabName = "home", icon = icon("home")),
                menuItem("Overview", tabName = "overview", icon = icon("area-chart")),
                menuItem("Turismo Interno", tabName = "tinterno", icon = icon("bed"))
                )
  )
  
  
}