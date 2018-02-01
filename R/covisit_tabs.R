require(data.table)
source("R/covisit.R")

poi <- fread("data/sardegna_covisitation_all_poi_id.csv")
locations = sort(unique(poi$origin))
presence <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")
municipalities <- presence$comune_name


create_tab <- function(tabname, plot_id1, plot_id2, check_group_id){
     
      
      tab <-  tabItem(
                tabName = tabname,
                fluidPage(
                        
                        fluidRow(
                                column(
                                        width = 3, 
                                        wellPanel(
                                                tags$fieldset(
                                                                checkboxGroupInput(check_group_id, "Punti di interesse", choices = locations, selected = locations[1:10])
                                                                #selectInput("origin_all", "POI", choices = locations, multiple = T, selected = locations[1:4])
                                                              )
                                                 )
                                       ),         
                                column(
                                        width = 8,
                                        div(h3("Co-visite tra punti di interesse (fonte Vodafone)"),
                                            h5("periodo: Settembre 2015 - Settembre 2016"),
                                            id="region_title"),
                                        wellPanel(
                                             HTML("Il termine <b>co-visita </b> indica la presenza dello stesso utente in una coppia di localit&agrave;, 
                                             indipendentemente dalla durata della visita e dalla direzione del movimento. 
                                             Esprime in sostanza una connessione tra due localit&agrave; o due punti di interesse.")
                                          
                                        ),
                                        plotlyOutput(plot_id1)
                                        # br(),
                                        # br(),
                                        # plotlyOutput(plot_id2)                                        
                                      )
                                 )
                        
                        
                )
        )
      return(tab)
}
# create_tab_bar <- function(tabname, label, plot_id1, input_id1, mun = municipalities){
# 
#         tab <-  tabItem(
#                 tabName = tabname,
#                 fluidPage(
#                         fluidRow(
#                                 column(
#                                         width = 3,
#                                         wellPanel(
#                                                 tags$fieldset(
#                                                         selectInput(input_id1, label, choices = mun, selected = mun[1])
#                                                 )
#                                         )
#                                 ),
#                                 column(
#                                         width = 8,
#                                         plotlyOutput(plot_id1)
#                                        )
#                                 )
#                           )
#                         )
#         return(tab)
# 
# }


