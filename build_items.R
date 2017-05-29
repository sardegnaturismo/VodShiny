require(ggplot2)
require(plotly)
source("R/destination_by_municipalities.R")
source("R/destination_by_provinces.R")
source("R/covisit_tabs.R")

dataset <- read.csv("data/sardegna_destinations_for_municipalities.csv")

provinces <- read.csv("data/sardegna_destinations_for_provinces.csv")
presenze_ita_prov <- read.csv("data/sardegna_presence_Sep15-Sep16_Italians_provinces.csv")
comuni <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")


build_items <- function(){

tabItems(          
  tabItem(
    tabName = "home",
    
    tags$div(
      tabName = "home",
      class="jumbotron text-center",
      tags$div(
        h1("Sardinia Smart Tourism"),
        tags$div(
          h4("Analisi del settore turistico sardo nel periodo Settembre 2015 - Settembre 2016"),
          br(),
          tags$img(src="home1.png", class="img-circle", size = "1000x800"),
          class="well"
        )
        
      ))),
  
        tabItem(
                tabName = "provenienze",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                                tags$fieldset(
                                                     tags$legend('Diagram 1', class = 'fieldlegend'),       
                                                     class = "fieldgroup",        
                                                     sliderInput("preset_it", "Regioni visualizzate", min=0, max= 5, value = 2.5, step = 0.5),
                                                     radioButtons("color1", "Visualizzazione pie chart 1:",
                                                                     c("Palette" = "palette", "Blue scale" = "blue"), selected = 'blue')                                                        
                                                        
                                                ),
                                                br(),
                                                br(),
                                                tags$fieldset(

                                                    tags$legend('Diagram 2', class = 'fieldlegend'),       
                                                    class = "fieldgroup",        
                                                    sliderInput("preset_st", "Nazioni visualizzate", min=0, max= 5, value = 2.5, step = 0.5),
                                                    radioButtons("color2", "Visualizzazione pie chart 2:",
                                                               c("Palette" = "palette", "Red scale" = "red"), selected = 'red')                                                        
                                                )
                                                

                                                )
                                       ),
                                column(
                                        width = 7,
                                        plotlyOutput("tot_it")
                                      ),
                                column(
                                        width = 7,
                                        plotlyOutput("tot_st")
                                )
                                
                                                  
                        ),
                        # ,br(),
                        # fluidRow(
                        #         column(width = 4),
                        #         column(
                        #                 width = 7,
                        #                 plotlyOutput("tot_st")
                        #         )                 
                        # ),
                        
                        # fluidRow(
                        #         column(
                        #                 width = 4,
                        #                 wellPanel(
                        #                         sliderInput("preset_st", "Provenienze visualizzate", min=2, max= 20, value = 8, step = 1),
                        #                         radioButtons("color", "Visualizzazione diagramma:",
                        #                                      c("Colore" = "colore", "Scala di grigio" = "grigio"), selected = 'colore')                                                
                        #                         )
                        #                 ),
                        #     
                        #           ),
                        
                        br(),
                        br()
           
                        # 
                        # fluidRow(
                        #         column(
                        #               width = 4,
                        #               wellPanel(
                        #                       checkboxGroupInput(
                        #                               "pres_control",
                        #                               "Presenze: ",
                        #                               choices = c("Residenti in Sardegna", "Massimo", "Minimo"),
                        #                               selected = "Residenti in Sardegna"
                        #                       )
                        #               )
                        #               
                        #         ),
                        #         column(
                        #                 width = 7
                        #                 plotlyOutput("residents")
                        #         )
                        #         
                        # ),
                        # fluidRow(
                        #         column(
                        #                 width = 4,
                        #                 wellPanel(
                        #                         checkboxGroupInput(
                        #                                 "vis_control",
                        #                                 "Presenze: ",
                        #                                 choices = c("Visitatori Italiani", "Visitatori Stranieri"),
                        #                                 selected = "Visitatori Italiani"
                        #                                 )
                        #                         )
                        #                 ),
                        #         column(
                        #                 width = 7,
                        #                 plotlyOutput("visitors")
                        #               )
                        #         )
                )
        ),
  
        tabItem(
                tabName = "presenze_it",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 8, offset = 2,
                                        plotlyOutput("residents")
                                ),br(),br(),
                                column(
                                        width = 8, offset = 2,
                                        wellPanel(id = "res_panel",
                                                tags$fieldset(
                                                        tags$legend('Residenti', class = 'fieldlegend'),
                                                        radioButtons(inputId = "res_control", label = "Colore: ", choices = c(Blue = "blue", Green = "darkgreen"),
                                                                     selected = "blue" )
                
                                                              )
                                                 )
                                ),br(),br(),br(),                                
                                
                                column(
                                        width = 8, offset = 2,
                                        plotlyOutput("visitors")
                                ),br(),br(),br(),
                                column(
                                        width = 8, offset = 2,
                                        wellPanel(
                                                id = "vis_panel",
                                                tags$fieldset(
                                                        tags$legend('Visitatori', class = 'fieldlegend'), 
                                                        checkboxGroupInput("vis_control",
                                                                           "Presenze: ",
                                                                           choices = c("Visitatori Italiani", "Visitatori Stranieri"),
                                                                           selected = "Visitatori Italiani"
                                                        )
                                                        
                                                )
                                        )                                        
                                )                                
                                
                        )
                )
        ),
  
        tabItem(
                tabName = "prov_interno",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                               
                                                selectInput("province1", "Provincia di destinazione:", choices = unique(sort(provinces[,2]))),
                                                radioButtons("diagram_type", "Scegli diagramma:",
                                                             c("Pie" = "pie", "Bar Chart" = "bar"), selected = 'pie')                                                
                                        )

                                        
                                ),
                                column(
                                        width = 7,
                                        plotlyOutput("destination_provinces")
                                )
                        ),
                        fluidRow(
                                # column(),
                                # column()
                        )
                )
                
        ),
  
        
        tabItem(
                tabName = "com_interno",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                                selectInput("municipality1", "Comune:", choices = unique(sort(dataset[,2]))),
                                                sliderInput("th", "Comuni visualizzati", min=4, max= 10, value = 6, step = 1),
                                                radioButtons("color", "Visualizzazione diagramma:",
                                                c("Colore" = "colore", "Scala di grigio" = "grigio"), selected = 'colore')                                                
                                        )


                                ),
                                column(
                                        width = 7,
                                        plotlyOutput("plot1")
                                       )
                                ),
                        br(),
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                                selectInput("municipality2", "Comune:", choices = unique(sort(dataset[,2])))        
                                        )
                                        
                                ),
                                column(
                                        width = 7,
                                        plotlyOutput("plot2")
                                )
                        )
                        )
                ),
            create_tab("cov_tot", "h_cov_tot", "h_cov_res", "origins1"),
            create_tab("cov_vis", "h_cov_it", "h_cov_st", "origins2"),
            tabItem(
                  tabName = "com_by_prov",
                  fluidPage(
                    fluidRow(
                        column(
                          width = 4, offset = 4,
                          wellPanel(
                            selectInput("municipality3", "Comune:", choices = unique(sort(comuni[["comune_name"]]))),
                            sliderInput("thresh2", "Percentage threshold:", min = 0, max = 5, value = 2.5, step = 0.5)                
                          )                    
                        ),
                        column(
                          width = 8, offset = 2,
                          plotlyOutput("com_prov_it")
                        ),br(), br(), br(),
                        column(
                          width = 8, offset = 2,
                          plotlyOutput("com_prov_st")
                         
                        )
                )
              )
              
            ),
  
  tabItem(
    tabName = "prov_it",
    fluidPage(
      fluidRow(
           column(id="all_provinces",
                  width = 12,
                  plotlyOutput("plot_prov")
                  ),br(), br(), br(),
      fluidRow(     
           column(
             width = 10, offset = 1, 
             wellPanel(id="prov_well",
               tags$fieldset(
                 class = "fieldgroup",
                 selectInput(inputId = "cat_prov", label = "Categoria", choices = c("Visitatori italiani", "Visitatori stranieri"), selected = "Visitatori italiani"),
                 sliderInput("prov_it1", "Regioni visualizzate", min=0, max= 5, value = 2.5, step = 0.5)
               )))
          )
           
      )))
  
  
  
  
                #create_tab_bar(tabname = "com_prov", label = "Comune di destinazione: ", plot_id1 = "plot_com_prov",input_id1 = "com_id")
                
             
        )
                
                # titlePanel("Destination by municipalities"),
                # sidebarLayout(
                #         sidebarPanel(
                #                 width = 4,
                #                 #dataset <- read.csv("data/sardegna_destinations_for_municipalities.csv"),
                #                 selectInput("municipality", "Municipality:", choices = unique(sort(dataset[,2]))),
                #                 sliderInput("th", "Comuni visualizzati", min=4, max= 10, value = 10, step = 1),
                #                 radioButtons("color", "Visualizzazione diagramma:",
                #                              c("Colore" = "colore",  
                #                                "Scala di grigio" = "grigio"))
                #         ),
                #         mainPanel(
                #                 width = 7,
                #                 plotOutput("plot1") 
                #         ),

                        
        #         )
        #         # h2("Destinations by municipalities"),
        #       
        # )                
        

  
  
}