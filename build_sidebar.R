build_sidebar <- function(){
  
  dashboardSidebar(
    sidebarMenu(id="items",
                menuItem("home", tabName = "home", icon = icon("home")),
                menuItem("Turismo", icon = icon("area-chart"),
                         menuSubItem("Regione: provenienze", tabName = "provenienze"),
                         menuSubItem("Province: provenienze", tabName = "prov_it"),                         
                         menuSubItem("Comuni: provenienze", tabName = "com_by_prov"),
                         menuSubItem("Provenienza nel periodo", tabName = "provenienze_tmp"),
                         menuSubItem("Presenza it e st", tabName = "presenze_it")),
                menuItem("Co-visite", icon = icon("share-alt-square"),
                         menuSubItem("Totali e Residenti", tabName = "cov_tot"),
                         # menuSubItem("Residenti", tabName = "cov_res"),
                         menuSubItem("Visitatori italiani e stranieri", tabName = "cov_vis")
                         
                ),
                         
                menuItem("Turismo Interno", icon = icon("bed"),
                         menuSubItem("Province", tabName = "prov_interno"),
                         menuSubItem("Comuni", tabName = "com_interno")
                         )
                

                ))
        
  
}