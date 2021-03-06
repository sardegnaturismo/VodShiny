require(data.table)

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
                                        h3("Co-visite tra Punti di interesse (fonte Vodafone)", id="region_title"),br(),br(),
                                        plotlyOutput(plot_id1),
                                        br(),
                                        br(),
                                        plotlyOutput(plot_id2)                                        
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


