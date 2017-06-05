build_sidebar <- function(){
  
  dashboardSidebar(
    sidebarMenu(id="items",
                menuItem("home", tabName = "home", icon = icon("home")),
                menuItem("Turismo", icon = icon("area-chart"),
                         menuItem("Regione", 
                                        menuSubItem("Provenienza visitatori", tabName = "provenienze"),
                                        menuSubItem("Presenze giornaliere", tabName = "presenze_regione")
                                     ),                                     
                         menuItem("Province", 
                                        menuSubItem("Provenienza visitatori", tabName = "prov_it"),
                                        menuSubItem("Cagliari", tabName = 'ca'),
                                        menuSubItem("Carbonia-Iglesias", tabName = 'ci'), 
                                        menuSubItem("Nuoro", tabName = 'nu'),                
                                        menuSubItem("Ogliastra", tabName = 'og'),                                  
                                        menuSubItem("Olbia-Tempio", tabName = 'ot'),
                                        menuSubItem("Sassari", tabName = 'ss'),
                                        menuSubItem("Medio Campidano", tabName = 'vs')),
                         menuItem("Comuni", 
                                        menuSubItem("Provenienza visitatori", tabName = "com_by_prov"))
                         ),
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