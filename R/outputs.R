render_province_it <- function(province, input){
       render_prov_it <- renderPlotly({
                it <- fread("data/sardegna_presence_Sep15-Sep16_Italians_provinces.csv")
                it_sired <- fread("data/sired_provenienze_italiani.csv")
                
                visitors <- get_italian_visitors_by_province(it, province, input$preset_ca_it)
                sired_visitors <- get_sired_italian_visitors_by_province(it_sired, province, input$preset_ca_it)
                # foreigners <- get_tot_foreigners_by_prov(st)
                col_it = colors
                if (input[["color_ca_1"]] == "diverging"){
                        col_it <- (colorRampPalette(brewer.pal(11, "Spectral"))(length(visitors$origin)))
                        col_it <- rev(col_it)
                }
                p <- plot_ly() %>%
                        add_pie(data = visitors, labels = ~origin, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", origin), hoverinfo = 'text', marker = list(colors = col_it, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = sired_visitors, labels = ~regions, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", regions), hoverinfo = 'text', marker = list(colors = col_it, line = list(color = '#FFFFFF')), domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(title =  paste("Provenienza dei visitatori italiani nella Provincia di", toupper(province)), annotations = list(
                                list(x = 0.20 , y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                                list(x = 0.80 , y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))
                p    
                
        })
        
        
       return(render_prov_it) 
        
        
}


render_province_st <- function(province, input){
 render_prov_st  <- renderPlotly({
         st <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
         st_sired <- fread("data/sired_provenienze_stranieri.csv")
         
         foreigners <- get_foreign_visitors_by_province(st, province, input$preset_ca_st)
         
         sired_foreigners <- get_sired_foreign_visitors_by_province(st_sired, province, input$preset_ca_st)
         # foreigners <- get_tot_foreigners_by_prov(st)
         col_st = colors
         if (input[["color_ca_2"]] == "diverging"){
                 col_st <- (colorRampPalette(brewer.pal(8, "Accent"))(length(foreigners$country)))
         }
         p <- plot_ly() %>%
                 add_pie(data = foreigners, labels = ~country, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", country), hoverinfo = 'text', marker = list(colors = col_st, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                 add_pie(data = sired_foreigners, labels = ~nations, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", nations), hoverinfo = 'text', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                 layout(title =  "Provenienza dei visitatori italiani nella Provincia di Cagliari", annotations = list(
                         list(x = 0.20 , y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                         list(x = 0.80 , y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))
         p    
         
 })
 
 return(render_prov_st)
}        

