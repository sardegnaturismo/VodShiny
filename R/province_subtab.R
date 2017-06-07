source("R/constants.R")
create_province_sub_tab <- function(province, plot_id){
        
        province_symbol = tolower(province_symbols[[province]])
        tab_id = paste(province_symbol, "_sub", sep = '')
        radio_button_id <- paste(tab_id, "_1")
        
        
        tab <- tabItem(
                tabName = tab_id,
                fluidPage(
                        h2(province, id="province_title"),
                        h3("Presenze giornaliere", id = "province_subtitle"),
                        fluidRow(
                                column(
                                        width = 2,
                                        wellPanel(
                                                tags$fieldset(
                                                        tags$legend('Dati', class = 'fieldlegend'),       
                                                        class = "fieldgroup",        
                                                        radioButtons(radio_button_id, "Fonte:",
                                                                     c("Vodafone" = "vodafone", "Sired" = "sired", "All" = "all"), selected = "all")                                                        
                                                        
                                                ))                                          
                                ),
                                column(
                                        width = 10,
                                        plotlyOutput(plot_id, inline = T)   
                                        
                                )
                                
                        )
                        
                )
        )
        
        return(tab)
        
        
}