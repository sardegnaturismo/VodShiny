build_sidebar <- function(){
  
  dashboardSidebar(
    sidebarMenu(id="items",
                menuItem("home", tabName = "home", icon = icon("home")),
                menuItem("Turismo", icon = icon("area-chart"),
                         menuSubItem("Provenienza visitatori", tabName = "provenienze"),
                         menuSubItem("Provenienza nel periodo", tabName = "provenienze_tmp"),
                         menuSubItem("Presenza it e st", tabName = "presenze_it"),
                         menuSubItem("Presenza nei Comuni", tabName = "com_prov")),
                         # menuSubItem("Presenza stranieri", tabName = "presenze_st")),
        
                menuItem("Turismo Interno", icon = icon("bed"),
                         menuSubItem("Province", tabName = "prov_interno"),
                         menuSubItem("Comuni", tabName = "com_interno")
                         ),
                
                menuItem("Co-visite", icon = icon("share-alt-square"),
                         menuSubItem("Totali e Residenti", tabName = "cov_tot"),
                         # menuSubItem("Residenti", tabName = "cov_res"),
                         menuSubItem("Visitatori italiani e stranieri", tabName = "cov_vis")
                        
                        )
                ))
        
  
}