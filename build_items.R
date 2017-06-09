require(ggplot2)
require(plotly)
source("R/destination_by_municipalities.R")
source("R/destination_by_provinces.R")
source("R/covisit_tabs.R")
source("R/province_tab.R")
source("R/province_subtab.R")

dataset <- read.csv2("data/sardegna_destinations_for_municipalities.csv")

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
        h3("Analisi del settore turistico sardo nel periodo Settembre 2015 - Settembre 2016"),br(),
        p("Lo studio utilizza dati SiRED e dati anonimi e aggregati della rete di Vodafone Italia, elaborati mediante metodi di data science per fornire una visione complessiva dei visitatori in Sardegna"),
        tags$div(
          br(),
          tags$img(src="home2.jpg", class="img-rounded"),
          class="well"
        )
        
      ))),
        tabItem(
                tabName = "provenienze",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 2,
                                        wellPanel(
                                                tags$fieldset(
                                                     tags$legend('Visitatori italiani', class = 'fieldlegend'),       
                                                     class = "fieldgroup",        
                                                     sliderInput("preset_it", "Soglia di visualizzazione Regioni (%)", min=1, max= 5, value = 2, step = 0.5),
                                                     radioButtons("color1", "Visualizzazione pie chart 1:",
                                                                     c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
                                                        
                                                ),
                                                br(),
                                                br(),
                                                tags$fieldset(

                                                    tags$legend('Visitatori stranieri', class = 'fieldlegend'),       
                                                    class = "fieldgroup",        
                                                    sliderInput("preset_st", "Soglia di visualizzazione Nazioni (%)", min=1, max= 5, value = 1, step = 0.5),
                                                    radioButtons("color2", "Visualizzazione pie chart 2:",
                                                               c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
                                                )
                                                

                                                )
                                       ),
                                column(
                                        width = 10,
                                        plotlyOutput("tot_it")
                                      ),
                                column(
                                        width = 10,
                                        plotlyOutput("tot_st")
                                )
                                
                                                  
                        )
        )),
        
        
                
  
        tabItem(
                tabName = "presenze_regione",
                fluidPage(
                  h3("Regione Sardegna", id="region_title"),
                  h6("presenze giornaliere", id = "province_subtitle"),                  
                        fluidRow(
                                # column(
                                #         width = 8, offset = 2,
                                #         plotlyOutput("residents")
                                # ),br(),br(),
                                # column(
                                #         width = 8, offset = 2,
                                #         wellPanel(id = "res_panel",
                                #                 tags$fieldset(
                                #                         tags$legend('Residenti', class = 'fieldlegend'),
                                #                         radioButtons(inputId = "res_control", label = "Colore: ", choices = c(Blue = "blue", Green = "darkgreen"),
                                #                                      selected = "blue" )
                                # 
                                #                               )
                                #                  )
                                # ),br(),br(),br(),                                
                                
                                # column(width = 2,
                                #        wellPanel(
                                #                id = "vis_panel",
                                #                tags$fieldset(
                                #                        tags$legend('Visitatori', class = 'fieldlegend'), 
                                #                        checkboxGroupInput("vis_control",
                                #                                           "Presenze: ",
                                #                                           choices = c("Dati Vodafone", "Dati Sired", "Visitatori Italiani", "Visitatori Stranieri"),
                                #                                           selected = c("Dati Vodafone", "Dati Sired", "Visitatori Italiani", "Visitatori Stranieri")
                                #        )))),
                                column(
                                        width = 10, offset = 1,
                                        plotlyOutput("visitors")
                                )),br(),br(),
                          fluidRow(      
                                column(
                                        width = 4, offset = 1,
                                        wellPanel(
                                                id = "vis_panel",
                                                tags$fieldset(
                                                        tags$legend('Visitatori', class = 'fieldlegend'),
                                                        checkboxGroupInput("vis_control",
                                                                           "Presenze: ",
                                                                           choices = c("Dati Vodafone", "Dati Sired", "Visitatori Italiani", "Visitatori Stranieri"),
                                                                           selected = c("Dati Vodafone", "Dati Sired", "Visitatori Italiani", "Visitatori Stranieri")
                                                                           )
                                                               )
                                                  )
                                        )
                                    )
                                
                                
                )
        ),
        create_province_tab("Cagliari", "ca_it", "ca_st"),
        create_province_sub_tab("Cagliari", "ca_sub_it"),
        create_province_tab("Oristano", "or_it", "or_st"),
        create_province_sub_tab("Oristano", "or_sub_it"),  
        create_province_tab("Nuoro", "nu_it", "nu_st"),
        create_province_sub_tab("Nuoro", "nu_sub_it"),
        create_province_tab("Ogliastra", "og_it", "og_st"),
        create_province_sub_tab("Ogliastra", "og_sub_it"),
        create_province_tab("Olbia-Tempio", "ot_it", "ot_st"),
        create_province_sub_tab("Olbia-Tempio", "ot_sub_it"),
        create_province_tab("Carbonia-Iglesias", "ci_it", "ci_st"),
        create_province_sub_tab("Carbonia-Iglesias", "ci_sub_it"),
        create_province_tab("Sassari", "ss_it", "ss_st"),
        create_province_sub_tab("Sassari", "ss_sub_it"),
        create_province_tab("Medio Campidano", "vs_it", "vs_st"),
        create_province_sub_tab("Medio Campidano", "vs_sub_it"),  
        tabItem(
                tabName = "prov_interno",
                fluidPage(
                        fluidRow(
                                column(
                                        width = 4,
                                        wellPanel(
                                               
                                                selectInput("province1", "Provincia di destinazione:", choices = unique(sort(provinces[,2]))),
                                                radioButtons("diagram_type", "Scegli diagramma:",
                                                             c("Pie" = "pie", "Bar Chart" = "bar"), selected = 'bar')                                                
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
                                # column(
                                #         width = 4,
                                #         wellPanel(
                                #                 selectInput("municipality2", "Comune:", choices = unique(sort(dataset[,2])))        
                                #         )
                                #         
                                # ),
                                column(
                                        width = 7, offset = 4,
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
                          width = 2, 
                          wellPanel(
                            selectInput("municipality3", "Comune:", choices = unique(sort(comuni[["comune_name"]]))),
                            sliderInput("thresh2", "Soglia di visualizzazione (%)", min = 1, max = 5, value = 2.5, step = 0.5)                
                          )                    
                        ),
                        column(
                          width = 8,
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
                  ),br(), br(), br(),br(), br(),
      fluidRow(     
           column(
             width = 9, offset = 1, 
             wellPanel(id="prov_well",
               tags$fieldset(
                 class = "fieldgroup",
                 selectInput(inputId = "cat_prov", label = "Categoria", choices = c("Visitatori italiani", "Visitatori stranieri"), selected = "Visitatori italiani"),
                 sliderInput("prov_it1", "Soglia di visualizzazione (%)", min=1, max= 5, value = 2.5, step = 0.5)
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