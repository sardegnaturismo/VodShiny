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
                                        menuItem("Overview", tabName = "prov_it"),
                                  
                                        menuItem("Cagliari", 
                                                 menuSubItem("Provenienze", tabName = 'ca'),
                                                 menuSubItem("Presenze", tabName = 'ca_sub')),
                                        menuItem("Carbonia-Iglesias", 
                                                   menuSubItem("Provenienze", tabName = 'ci'),
                                                   menuSubItem("Presenze", tabName = 'ci_sub')),                                  
                                        menuItem("Nuoro", 
                                                  menuSubItem("Provenienze", tabName = 'nu'),
                                                  menuSubItem("Presenze", tabName = 'nu_sub')),                                  
                                        menuItem("Ogliastra", 
                                                   menuSubItem("Provenienze", tabName = 'og'),
                                                   menuSubItem("Presenze", tabName = 'og_sub')),
                                        menuItem("Olbia-Tempio", 
                                                   menuSubItem("Provenienze", tabName = 'ot'),
                                                   menuSubItem("Presenze", tabName = 'ot_sub')),                                  
                                        menuItem("Oristano", 
                                                   menuSubItem("Provenienze", tabName = 'or'),
                                                   menuSubItem("Presenze", tabName = 'or_sub')),                                  
                                        menuItem("Sassari", 
                                                   menuSubItem("Provenienze", tabName = 'ss'),
                                                   menuSubItem("Presenze", tabName = 'ss_sub')),                                  
                                        menuItem("Medio Campidano", 
                                                   menuSubItem("Provenienze", tabName = 'vs'),
                                                   menuSubItem("Presenze", tabName = 'vs_sub'))),                                  

                        menuItem("Comuni", 
                                        menuSubItem("Provenienza visitatori", tabName = "com_by_prov"))),
        menuItem("Co-visite", icon = icon("share-alt-square"),
                menuSubItem("Totali e Residenti", tabName = "cov_tot"),
                menuSubItem("Visitatori italiani e stranieri", tabName = "cov_vis")
                ),
                         
        menuItem("Turismo Interno", icon = icon("bed"),
                menuSubItem("Province", tabName = "prov_interno"),
                menuSubItem("Comuni", tabName = "com_interno")
                         )

                ))
        
  
}