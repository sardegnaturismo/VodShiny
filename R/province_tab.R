source("R/constants.R")
require(ggplot2)

create_province_tab <- function(province, it_diagram, st_diagram){
  province_symbol = tolower(province_symbols[[province]])
  
  slider_id1 <- paste("preset_", province_symbol, "_it", sep = '' )
  slider_id2 <- paste("preset_", province_symbol, "_st", sep = '' )  
  radio_button_id1 <- paste("color_", province_symbol, "_1", sep = '')
  radio_button_id2 <- paste("color_", province_symbol, "_2", sep = '')
  
  

  # print(slider_id1)
  # print(slider_id2)
  # print(radio_button_id1)
  # print(radio_button_id2)
  
   tab <- tabItem(
        tabName = province_symbol,
    tabsetPanel(
          tabPanel(
                  title = province_symbol,
                  
                  fluidPage(
                          h2(id= "province_title", province),
                          fluidRow(
                                  column(
                                          width = 2,
                                          
                                          wellPanel(
                                                  tags$fieldset(
                                                          tags$legend('Visitatori italiani', class = 'fieldlegend'),       
                                                          class = "fieldgroup",        
                                                          sliderInput(slider_id1, "Regioni visualizzate", min=1, max= 5, value = 2, step = 0.5),
                                                          radioButtons(radio_button_id1, "Visualizzazione pie chart 1:",
                                                                       c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
                                                          
                                                  ),
                                                  br(),
                                                  br(),
                                                  tags$fieldset(
                                                          
                                                          tags$legend('Visitatori stranieri', class = 'fieldlegend'),       
                                                          class = "fieldgroup",        
                                                          sliderInput(slider_id2, "Nazioni visualizzate", min=1, max= 5, value = 1, step = 0.5),
                                                          radioButtons(radio_button_id2, "Visualizzazione pie chart 2:",
                                                                       c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
                                                  )
                                                  
                                                  
                                          )
                                          
                                  ),
                                  column(
                                          width = 10,
                                          plotlyOutput(it_diagram, inline = T)
                                  ),
                                  column(
                                          width = 10,
                                          plotlyOutput(st_diagram, inline = T)
                                  )
                                  
                          )
                          
                  )),
          tabPanel(
                  title = "daily presence",
                  fluidPage(
                          h2("Daily presences", id="province_title"),
                          fluidRow(
                                  column(
                                          width = 2,
                                          wellPanel(
                                                  tags$fieldset(
                                                          tags$legend('Dati', class = 'fieldlegend'),       
                                                          class = "fieldgroup",        
                                                          radioButtons('aaaa', "Fonte:",
                                                                       c("Vodafone" = "vodafone", "Sired" = "sired", "All" = "all"))                                                        
                                                          
                                                  ))                                          
                                  ),
                                  column(
                                          width = 6
                                          #plotOutput("abcd", inline = T)   

                                  )
                                  
                          )
                          
                  )
                  
          )
          
          
  ))
  # tabItem(
  #   tabName = province_symbol,
  #  
  #   fluidPage(
  #     h2(id= "province_title", province),
  #     fluidRow(
  #       column(
  #         width = 2,
  # 
  #         wellPanel(
  #           tags$fieldset(
  #             tags$legend('Visitatori italiani', class = 'fieldlegend'),       
  #             class = "fieldgroup",        
  #             sliderInput(slider_id1, "Regioni visualizzate", min=1, max= 5, value = 2, step = 0.5),
  #             radioButtons(radio_button_id1, "Visualizzazione pie chart 1:",
  #                          c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
  #             
  #           ),
  #           br(),
  #           br(),
  #           tags$fieldset(
  #             
  #             tags$legend('Visitatori stranieri', class = 'fieldlegend'),       
  #             class = "fieldgroup",        
  #             sliderInput(slider_id2, "Nazioni visualizzate", min=1, max= 5, value = 1, step = 0.5),
  #             radioButtons(radio_button_id2, "Visualizzazione pie chart 2:",
  #                          c("Palette" = "diverging", "Standard" = "standard"), selected = 'diverging')                                                        
  #           )
  #           
  #           
  #         )
  #       
  #       ),
  #       column(
  #         width = 10,
  #         plotlyOutput(it_diagram, inline = T)
  #       ),
  #       column(
  #         width = 10,
  #         #plotlyOutput("abcd")
  #         plotlyOutput(st_diagram, inline = T)
  #       )
  #       
  #     )
  #    
  #   ))  
  return(tab)
}







