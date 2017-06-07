render_province_it <- function(province, input){
      province_symbol = tolower(province_symbols[[province]])  
      slider_id <- paste("preset_", province_symbol, "_it", sep = '' )  
      radio_button_id <- paste("color_", province_symbol, "_1", sep = '')
  
 
      
       render_prov_it <- renderPlotly({
                
                it <- fread("data/sardegna_presence_Sep15-Sep16_Italians_provinces.csv")
                it_sired <- fread("data/sired_provenienze_italiani.csv")
                
                visitors <- get_italian_visitors_by_province(it, province, input[[slider_id]])
                sired_visitors <- get_sired_italian_visitors_by_province(it_sired, province, input[[slider_id]])
                # foreigners <- get_tot_foreigners_by_prov(st)
                col_it = colors
                if (input[[radio_button_id]] == "diverging"){
                        col_it <- (colorRampPalette(brewer.pal(11, "Spectral"))(length(visitors$origin)))
                        col_it <- rev(col_it)
                }
                p <- plot_ly() %>%
                        add_pie(data = visitors, labels = ~origin, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", origin), hoverinfo = 'text', marker = list(colors = col_it, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                        add_pie(data = sired_visitors, labels = ~regions, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", regions), hoverinfo = 'text', marker = list(colors = col_it, line = list(color = '#FFFFFF')), domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                        layout(title =  paste("Provenienza dei visitatori italiani"), annotations = list(
                                list(x = 0.20 , y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                                list(x = 0.80 , y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))
                p
                
                
        })
        
        
       return(render_prov_it) 
        
        
}


render_province_st <- function(province, input){
  
  province_symbol = tolower(province_symbols[[province]])
  
  slider_id <- paste("preset_", province_symbol, "_st", sep = '' )  
  radio_button_id <- paste("color_", province_symbol, "_2", sep = '')
  
  
  
  
  
  
 render_prov_st  <- renderPlotly({
         
         
         st <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
         st_sired <- fread("data/sired_provenienze_stranieri.csv")
         
         foreigners <- get_foreign_visitors_by_province(st, province, input[[slider_id]])
         
         sired_foreigners <- get_sired_foreign_visitors_by_province(st_sired, province, input[[slider_id]])
         # foreigners <- get_tot_foreigners_by_prov(st)
         col_st = colors
         if (input[[radio_button_id]] == "diverging"){
                 col_st <- (colorRampPalette(brewer.pal(8, "Accent"))(length(foreigners$country)))
         }
         p <- plot_ly() %>%
                 add_pie(data = foreigners, labels = ~country, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", country), hoverinfo = 'text', marker = list(colors = col_st, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                 add_pie(data = sired_foreigners, labels = ~nations, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", nations), hoverinfo = 'text', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                 layout(title =  "Provenienza dei visitatori stranieri", annotations = list(
                         list(x = 0.20 , y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                         list(x = 0.80 , y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))
         p    
         
 })
 
 return(render_prov_st)
}

province_curve <- function(province, input){
        
        province_symbol = tolower(province_symbols[[province]])
        tab_id = paste(province_symbol, "_sub", sep = '')
        radio_button_id <- paste(tab_id, "_1")        
  
 
  x <- renderPlotly({
         
          
    italians <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")
    foreigners <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
    sired_daily <- fread("data/sired_daily_all.csv")

    visitors <- get_presence_visitors_by_prov(italians, province = province)
    strangers <- get_presence_foreigners_by_prov(foreigners, province = province)
    
        
    sired_italians <- get_sired_daily_by_prov_it(sired_daily, province = province)
    sired_foreigners <- get_sired_daily_by_prov_st(sired_daily, province = province)
    
    
    p = NULL
    s = NULL
    

  
      visitors$Presenze <- "Italiani (dati Vodafone)"
      strangers$Presenze <- "Stranieri (dati Vodafone)"
      sired_italians$Presenze <- "Italiani (dati Sired)"
      sired_foreigners$Presenze <- "Stranieri (dati Sired)"
      
      leg <- NULL
      if(input[[radio_button_id]] == "vodafone"){
              all_visitors <- rbind(visitors, strangers)
              leg <- c("Italiani (dati Vodafone)" = "blue", "Stranieri (dati Vodafone)" = "red")
              y_limits <- c(1000, 750000)
              
      }else if (input[[radio_button_id]] == "sired"){
              all_visitors <- rbind(sired_italians, sired_foreigners)
              leg <- c("Italiani (dati Sired)" = "orange", "Stranieri (dati Sired)" = "green")
              y_limits <- c(0, 25000)
              
      }else{
              all_visitors <- rbind(visitors, strangers, sired_italians, sired_foreigners)
              leg <- c("Italiani (dati Vodafone)" = "blue", "Stranieri (dati Vodafone)" = "red", "Italiani (dati Sired)" = "orange", "Stranieri (dati Sired)" = "green")
              y_limits <- c(0,750000)
      }
      
      
 
      p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(y_limits)  +
      scale_colour_manual(values = leg) + theme_minimal(base_size = 8)
      # 
      p.labs <- p + labs(x = "periodo", y = "")
      s <- p.labs
      ggplotly(s)

   
  })
  
 return(x)
  
}
