require(shiny)
require(data.table)
require(shinydashboard)
require(data.table)
require(dplyr)
require(plotly)
require(scales)
require(RColorBrewer)
source("R/destination_by_municipalities.R")
source("R/destination_by_provinces.R")
source("R/presence.R")
source("R/covisit.R")
source("R/utilities.R")
source("R/sired.R")
source("R/outputs.R")
source("R/validation.R")




shinyServer(function(input, output, session) {
  
        #### Destinations by municipality #####
        output$plot1 <- renderPlotly({
                dataset <- fread("data/sardegna_destinations_for_municipalities.csv", encoding = 'Latin-1')
                selected_data <- destination_by_municipalities(dataset, municipality_name = input$municipality1)
                print(selected_data)
                #par(mar = c(5,6,4,2))
                
                if (input$color == 'colore'){
                        selected_color = 'rgb(158,202,225)'
                        line_color = 'rgb(8,48,107)'
                }else{
                        selected_color = 'rgb(211,211,211)'
                        line_color = 'rbg(112,128,144)'
                }

                #print(selected_color)
                #barplot(height = selected_data[1:input$th,2], names.arg = abbreviate(selected_data[1:input$th,1], minlength = 10), main = input$municipality1, xlab = "Numero medio di visitatori", las=1, horiz = T, col=selected_color)
                labels <- filter_municipalities(selected_data[1:input$th, 1])
                
                
                p <- plot_ly(
                        data = selected_data,
                        y = labels,
                        x = selected_data[1:input$th,2],
                        type = "bar",
                        orientation = 'h',
                        marker = list(color = selected_color,
                                      line = list(color = line_color,
                                                  width = 1.5))
                        # marker = list(color = selected_color)
                ) %>% layout(title = paste("Comune di destinazione: ", input$municipality1, "(fonte Vodafone)"), yaxis = list(tickfont = list(size = 7, color = 'black')), xaxis = list(title="Numero medio di visitatori", tickfont = list(size = 8)))        
                p
                
                })
        
        output$plot2 <- renderPlotly({
                dataset <- fread("data/sardegna_destinations_for_municipalities.csv", encoding = 'Latin-1')
                selected_data <- destination_by_month(dataset, municipality_name = input$municipality1)
                # plot(x = 1:13, y = selected_data$visitors, type = 'o', xaxt = 'n', ylab = "Visitatori", xlab = "Periodo", col = 'blue', main = input$municipality2)
                # grid()
                # axis(1, at = 1:13, labels = selected_data$period)
                p <- plot_ly(data = selected_data, x = ~period, y = ~visitors, mode = 'lines+markers', type = 'scatter')
                #layout(title = paste('Comune di destinazione: ', input$municipality2), )        
                        
                
        })
        
        
        output$destination_provinces <- renderPlotly({
           
          provinces <- fread("data/sardegna_destinations_for_provinces.csv")               
          province_data <- destination_by_provinces(provinces, province_name = input$province1)
          print(input$diagram_type)
          # par(mar = c(10,14,4,2))
          prov_symbols = get_province_symbols(province_data[,1])
          
          if (input$diagram_type == "pie"){
                pct = round(100*province_data[,2]/sum(province_data[,2]), digits = 2)
                lbls <- paste(prov_symbols, pct) # add percents to labels 
                lbls <- paste(lbls,"%",sep="") # ad % to labels 
                # par(bg='transparent')
                # pie(province_data[,2], labels = lbls, main = input$province1)
                p <- plot_ly(province_data, labels = ~origin, values = ~visitors, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(origin, ":", visitors), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        layout(title = paste("Provincia di Destinazione: ", input$province1, "(fonte Vodafone)"), showlegend = T)                
          }else{
                  # par(bg = 'transparent')
                  # barplot(height = province_data[,2], names.arg = prov_symbols, main = input$province1, col = c(rainbow(length(province_data[,1]))),
                  #         xlab = "Numero di visitatori", ylab = "Province di origine", horiz = T
                selected_color = c(rainbow(length(province_data[,1])))  
                p <- plot_ly(data = province_data, x = ~origin, y = ~visitors, type = 'bar', marker = list(color = selected_color)) %>%
                        layout(title = paste("Provincia di Destinazione: ", input$province1, "(fonte Vodafone)"),  
                               yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Provincia di provenienza", tickfont = list(size = 8)))
          }
          
          
        })
        
        
        output$residents <- renderPlotly({
                italians <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")
                residents <- get_presence_residents(italians)
                print(input$res_control)
                selected_color <- input$res_control        
                p <- ggplot(data = residents, aes(date, presence)) + geom_line(colour=selected_color) + ylim(1500000, 1700000) + xlab("Periodo di tempo") + ylab("Presenze\n\n\n\n\n") + ggtitle("Residenti in Sardegna") + 
                         theme_minimal(base_size = 8)
                         # theme(axis.title.y = element_text(size=8), axis.title.x = element_text(size = 8),  
                         #       plot.margin = unit(c(0,0,0,1.2), 'lines'))
                plotly::ggplotly(p)
        })
        
        
        output$visitors <- renderPlotly({
                italians <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")
                foreigners <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
                sired_daily <- fread("data/sired_daily_all.csv")
                
                sired_italians <- get_sired_daily_all_it(sired_daily)
                sired_foreigners <- get_sired_daily_all_st(sired_daily)
                
                visitors <- get_presence_visitors(italians)
                strangers <- get_presence_foreigners(foreigners)
                
                p = NULL
                s = NULL
                
                print(length(input$vis_control))
                print(input$vis_control)
                print(is.null(input$vis_control))
                
                if (is.null(input$vis_control)){
                        visitors$Presenze <- "Italiani (dati Vodafone)"
                        strangers$Presenze <- "Stranieri (dati Vodafone)"
                        sired_italians$Presenze <- "Italiani (dati Sired)"
                        sired_foreigners$Presenze <- "Stranieri (dati Sired)"
                        all_visitors <- rbind(visitors, strangers, sired_italians, sired_foreigners)
                                
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(1000, 750000) +
                                        scale_colour_manual(values = c("Italiani (dati Vodafone)" = "blue", "Stranieri (dati Vodafone)" = "red", "Italiani (dati Sired)" = "orange", "Stranieri (dati Sired)" = "green" )) + theme_minimal(base_size = 8) 
                                
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs                        
                        
                        
                        
                } else if(input$vis_control == "Dati Vodafone" && length(input$vis_control) == 1 || (all(c("Visitatori Italiani", "Visitatori Stranieri", "Dati Vodafone") %in% input$vis_control) && length(input$vis_control) == 3)){
                        visitors$Presenze <- "Italiani (dati Vodafone)"
                        strangers$Presenze <- "Stranieri (dati Vodafone)"
                        all_visitors <- rbind(visitors, strangers)
                        
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(1000, 750000) +
                                scale_colour_manual(values = c("Italiani (dati Vodafone)" = "blue", "Stranieri (dati Vodafone)" = "red")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs
                }else if (((input$vis_control == "Dati Sired") & (length(input$vis_control) == 1)) | (all(c("Visitatori Italiani", "Visitatori Stranieri", "Dati Sired") %in% input$vis_control) & length(input$vis_control) == 3)){
                        sired_italians$Presenze <- "Italiani (dati Sired)"
                        sired_foreigners$Presenze <- "Stranieri (dati Sired)"
                        
                        all_visitors <- rbind(sired_italians, sired_foreigners)
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 95000) +
                                scale_colour_manual(values = c("Italiani (dati Sired)" = "green", "Stranieri (dati Sired)" = "orange")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs
                }else if ((all(c("Visitatori Italiani", "Dati Vodafone") %in% input$vis_control)) & length(input$vis_control) == 2){
                        visitors$Presenze <- "Italiani (dati Vodafone)"
                        all_visitors <- visitors
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 750000) +
                                scale_colour_manual(values = c("Italiani (dati Vodafone)" = "blue")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs                        

                }else if ((all(c("Visitatori Stranieri", "Dati Vodafone") %in% input$vis_control)) & length(input$vis_control) == 2){
                        strangers$Presenze <- "Stranieri (dati Vodafone)"
                        all_visitors <- strangers
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 250000) +
                                scale_colour_manual(values = c("Stranieri (dati Vodafone)" = "red")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs
                        
                }else if ((all(c("Visitatori Italiani", "Dati Sired") %in% input$vis_control)) & length(input$vis_control) == 2){
                        sired_italians$Presenze <- "Italiani (dati Sired)"
                        all_visitors <- sired_italians
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 95000) +
                                scale_colour_manual(values = c("Italiani (dati Sired)" = "orange")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs                        
                        
                }else if ((all(c("Visitatori Stranieri", "Dati Sired") %in% input$vis_control)) & length(input$vis_control) == 2){
                        sired_foreigners$Presenze <- "Stranieri (dati Sired)"
                        all_visitors <- sired_foreigners
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 95000) +
                                scale_colour_manual(values = c("Stranieri (dati Sired)" = "green")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs                        
                        
                }else if ((input$vis_control == 'Visitatori Italiani') & (length(input$vis_control) == 1) || (all(c("Visitatori Italiani", "Dati Vodafone", "Dati Sired") %in% input$vis_control)) & length(input$vis_control) == 3){
                        visitors$Presenze <- "Italiani (dati Vodafone)"
                        sired_italians$Presenze <- "Italiani (dati Sired)" 
                        all_visitors <- rbind(visitors, sired_italians)                        
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 700000) +
                                scale_colour_manual(values = c("Italiani (dati Vodafone)" = "blue", "Italiani (dati Sired)" = "orange")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs
                        
                        
                }else if ((input$vis_control == 'Visitatori Stranieri') & (length(input$vis_control) == 1) || (all(c("Visitatori Stranieri", "Dati Vodafone", "Dati Sired") %in% input$vis_control)) & length(input$vis_control) == 3){
                        strangers$Presenze <- "Stranieri (dati Vodafone)"
                        sired_foreigners$Presenze <- "Stranieri (dati Sired)"
                        all_visitors <- rbind(strangers, sired_foreigners)                        
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(0, 200000) +
                                scale_colour_manual(values = c("Stranieri (dati Vodafone)" = "red", "Stranieri (dati Sired)" = "green")) + theme_minimal(base_size = 8) 
                        
                        # p.labs <- p + labs(x = "periodo", y = "")
                        # s <- p.labs 
    
                }else{
                        visitors$Presenze <- "Italiani (dati Vodafone)"
                        strangers$Presenze <- "Stranieri (dati Vodafone)"
                        sired_italians$Presenze <- "Italiani (dati Sired)"
                        sired_foreigners$Presenze <- "Stranieri (dati Sired)"
                        all_visitors <- rbind(visitors, strangers, sired_italians, sired_foreigners)
                        
                        p = ggplot(data = all_visitors, aes(date, presence)) + geom_line(aes(colour=Presenze)) + ylim(1000, 750000) +
                                scale_colour_manual(values = c("Italiani (dati Vodafone)" = "blue", "Stranieri (dati Vodafone)" = "red", "Italiani (dati Sired)" = "orange", "Stranieri (dati Sired)" = "green" )) + theme_minimal(base_size = 8) 
                        
                        
                }
                    
                    # theme(axis.title.y = element_text(size=8), axis.title.x = element_text(size = 8),  
                            #       plot.margin = unit(c(0,0,0,1.2), 'lines'))      #theme_minimal()

                   
                    p.labs <- p + scale_y_continuous(labels = comma) + labs(x = "periodo", y = "")
                    s <- p.labs
                    ggplotly(s)
                    

                })
                
                
                
                output$tot_it <-renderPlotly({
                        it <- fread("data/sardegna_presence_Sep15-Sep16_Italians_provinces.csv")
                        it_sired <- fread("data/sired_provenienze_italiani.csv")
                        # st <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
                        visitors <- get_tot_visitors_by_prov2(it, input$preset_it)
                        sired_visitors <- get_sired_visitors_it(it_sired, input$preset_it)
                        # foreigners <- get_tot_foreigners_by_prov(st)
                        col_it = colors
                        if (input$color1 == "diverging"){
                            col_it <- (colorRampPalette(brewer.pal(12, "Spectral"))(length(visitors$origin)))
                            col_it <- rev(col_it)
                        }
                        
                        # #### single plot ###              
                        # p <- plot_ly(visitors, labels = ~origin, values = ~presence, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                        #              text = ~paste(origin, ":", presence), marker = list(colors = col_it, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        #      layout(title = "Provenienza dei visitatori italiani in Sardegna", showlegend = T)
                        ####################
                        #### multiple plot ####
                        p <- plot_ly() %>%
                          add_pie(data = visitors, labels = ~origin, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", origin), hoverinfo = 'text', marker = list(colors = col_it, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                          add_pie(data = sired_visitors, labels = ~regions, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", regions), hoverinfo = 'text', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                          layout(title =  "Provenienza dei visitatori italiani in Sardegna", annotations = list(
                            list(x = 0.20, y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                            list(x = 0.80, y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))
                        #############    
                        p    
                        
                        
                        # p <- plot_ly(foreigners, labels = ~country, values = ~presence, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                        #              text = ~paste(country, ":", presence), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        #      layout(title = "Provenienza dei visitatori italiani in Sardegna", showlegend = T)
                
                        
                        })
                
                output$tot_st <- renderPlotly({

                    st <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
                    sired_st <- fread("data/sired_provenienze_stranieri.csv")
                    
                    foreigners <- get_tot_foreigners_by_prov2(st, input$preset_st)
                    sired_foreigners <- get_sired_visitors_st(sired_st, input$preset_st)
                    
                    col_st = colors
                    if (input$color2 == "diverging"){
                      col_st <- (colorRampPalette(brewer.pal(8, "Accent"))(length(foreigners$country)))
                    }
                 ###single plot ###   
                    # p <- plot_ly(foreigners, labels = ~country, values = ~presence, type = 'pie', textinfo = 'percent', hoverinfo = 'text', text = ~paste(country, ":", presence), marker = list(colors = col_st, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                    #   layout(title = "Provenienza dei visitatori stranieri in Sardegna", showlegend = T)
                #####################
                    p <- plot_ly() %>%
                      add_pie(data = foreigners, labels = ~country, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", country), hoverinfo = 'text',  marker = list(colors = col_st, line = list(color = '#FFFFFF')), domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
                      add_pie(data = sired_foreigners, labels = ~nations, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", nations), hoverinfo = 'text', domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
                      layout(title =  "Provenienza dei visitatori stranieri in Sardegna", annotations = list(
                        list(x = 0.20, y = 1.15, text = "Dati Vodafone", showarrow = F, xref='paper', yref='paper'),
                        list(x = 0.80, y = 1.15, text = "Dati Sired", showarrow = F, xref='paper', yref='paper')))                    
                    
                    

                        
                })
                
                
                
                
                output$h_cov_tot <- renderPlotly({
                        cov_data <- fread("data/sardegna_covisitation_all_poi_id.csv")
                        origins <- input[["origins1"]]
                       # destinations <- input$dest_all
                        localities <- origins
                        od <- NULL
                        if (length(localities) > 1){
                           od <- get_covisits(dataset = cov_data, cust_class = NULL, chosen_localities = localities)                          
                        }
                        
                        p <- plot_ly(x = abbreviate(colnames(od),12), y = abbreviate(row.names(od),12), z = od, colors = colorRamp(c("blue", "red")), type = "heatmap") %>%
                        layout(title = "Co-visite Totali (fonte Vodafone)", yaxis = list(tickfont = list(size = 8)), xaxis = list(title="POI", tickfont = list(size = 8)))        
        
                        
                        
                })
                
                output$h_cov_res <- renderPlotly({
                        cov_data <- fread("data/sardegna_covisitation_all_poi_id.csv")
                        origins <- input[["origins1"]]
                        # destinations <- input$dest_all
                        localities <- origins
                        od <- NULL
                        if (length(localities) > 1){
                                od <- get_covisits(cov_data, "resident", localities)
                        }

                        p <- plot_ly(x = abbreviate(colnames(od),12), y = abbreviate(row.names(od),12), z = od, type = "heatmap") %>%
                                layout(title = "Co-visite Residenti (fonte Vodafone)", yaxis = list(tickfont = list(size = 8)), xaxis = list(title="POI", tickfont = list(size = 8)))

                })
                
                output$h_cov_it <- renderPlotly({
                        cov_data <- fread("data/sardegna_covisitation_all_poi_id.csv")
                        localities <- input[["origins2"]]
                        od <- NULL
                        if (length(localities) > 1){
                                od <- get_covisits(cov_data, "visitor", localities)
                        }
                        
                        p <- plot_ly(x = abbreviate(colnames(od),12), y = abbreviate(row.names(od),12), z = od, colors = colorRamp(c("darkgreen", "white")), type = "heatmap") %>%
                                layout(title = "Co-visite Visitatori Italiani (fonte Vodafone)", yaxis = list(tickfont = list(size = 8)), xaxis = list(title="POI", tickfont = list(size = 8)))
                })
                
                
                output$h_cov_st <- renderPlotly({
                        cov_data <- fread("data/sardegna_covisitation_all_poi_id.csv")
                        localities <- input[["origins2"]]
                        od <- NULL
                        if (length(localities) > 1){
                                od <- get_covisits(cov_data, "foreign", localities)
                        }
                        
                        p <- plot_ly(x = abbreviate(colnames(od),12), y = abbreviate(row.names(od),12), z = od, colors = colorRamp(c("purple", "white")), type = "heatmap") %>%
                                layout(title = "Co-visite Visitatori Stranieri (fonte Vodafone)", yaxis = list(tickfont = list(size = 8)), xaxis = list(title="POI", tickfont = list(size = 8)))                        
                        
                        
                })
                
                output$com_prov_it <- renderPlotly({
                        italians <- fread("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")
                        ###here we filter by municipality

                        filtered_italians <- filter(italians, comune_name == input$municipality3 & customer_class == "visitor")
                        
                        
                        # presets <- length(unique(filtered_italians$origin))
                        
                        visitors <- get_tot_visitors_by_prov2(filtered_italians, perc = input$thresh2)
                 
                        selected_color = rev(colorRampPalette(brewer.pal(9, "Reds"))(length(visitors$origin)))
                        p <- plot_ly(data = visitors, x = ~origin, y = ~presence, type = 'bar', marker = list(color = selected_color)) %>%
                                layout(title = paste("Comune di Destinazione: ", input$municipality3, "(fonte Vodafone)"),
                                       yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Regione di provenienza", tickfont = list(size = 8)))


                })
                
                output$com_prov_st <- renderPlotly({
                  strangers <- fread("data/Sardegna_presence_Sep15-Sep16_per_comune_Foreigners - version20161118.csv")
                  filtered_strangers <- filter(strangers, comune_name == input$municipality3)
                  
                  foreigners <- get_tot_foreigners_by_prov2(filtered_strangers, perc = input$thresh2)
                  selected_color = rev(colorRampPalette(brewer.pal(9, "Blues"))(length(foreigners$country)))
                  p <- plot_ly(data = foreigners, x = ~country, y = ~presence, type = 'bar', marker = list(color = selected_color)) %>%
                    layout(yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Nazione di provenienza", tickfont = list(size = 8)))                  
                  
                  
                })
                
                output$plot_prov <- renderPlotly({

                 
                  italians <- fread("data/sardegna_presence_Sep15-Sep16_Italians_provinces.csv")              
                  provinces = unique(italians$province)
                  visitors_list = list()
                  origin_list = list()
                  
                  
                  
                  if (input$cat_prov == "Visitatori italiani"){
                    for (i in 1:length(provinces)){
    
                      filtered_visitors <- filter(italians, customer_class == 'visitor' & province == provinces[i])
                      visitors_list[[i]] <- get_tot_visitors_by_prov2(filtered_visitors, perc = input$prov_it1)
                      origin_list[[i]] <- visitors_list[[i]]["origin"]
                      print(origin_list[[i]])
                      # selected_colors[[i]] <- (colorRampPalette(brewer.pal(9, "Reds"))(length(visitors_list[[i]]$origin))) 
                    }
                    
                  }else if (input$cat_prov == "Visitatori stranieri"){
                    for (i in 1:length(provinces)){
                      foreigners <- fread("data/sardegna_presence_Sep15-Sep16_foreigners_provinces.csv")
                      filtered_foreigners <- filter(foreigners, province == provinces[i])
                      visitors_list[[i]] <- get_tot_foreigners_by_prov2(filtered_foreigners, perc = input$prov_it1)
                      # selected_colors[[i]] <- (colorRampPalette(brewer.pal(9, "Reds"))(length(visitors_list[[i]]$country))) 
                    }
                    
                  }
         
                  indipent_var = NULL
                  title = NULL
                  selected_colors <- NULL
                  
                  if(input$cat_prov == "Visitatori italiani"){
                    independent_var = ~origin
                    title = "Provenienza dei visitatori italiani per provincia (fonte Vodafone)"
                    selected_colors = colorRampPalette(brewer.pal(8, "Accent"))(20)
                    #selected_colors <- (colorRampPalette(brewer.pal(9, "Reds"))(length(visitors_list[[1]]$origin)))
                    
                  }else{
                    independent_var = ~country
                    title = "Provenienza dei visitatori stranieri per provincia (fonte Vodafone)"
                    selected_colors = NULL
                    #selected_colors <- (colorRampPalette(brewer.pal(9, "Blues"))(length(visitors_list[[1]]$country)))
                  }
                  p <- plot_ly() %>%
                  add_pie(data = visitors_list[[1]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[1]][,1]), hoverinfo = 'text', marker = list(colors = selected_colors, line = list(color = '#FFFFFF', width = 1)), domain = list(x = c(0, 0.25), y = c(0.55, 1))) %>%
                  add_pie(data = visitors_list[[2]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[2]][,1]),hoverinfo = 'text', domain = list(x = c(0.25, 0.50), y = c(0.55, 1))) %>%
                  add_pie(data = visitors_list[[3]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[3]][,1]),hoverinfo = 'text', domain = list(x = c(0.50, 0.75), y = c(0.55, 1))) %>%
                  add_pie(data = visitors_list[[4]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[4]][,1]),hoverinfo = 'text', domain = list(x = c(0.75, 1), y = c(0.55, 1))) %>%
                  add_pie(data = visitors_list[[5]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[5]][,1]),hoverinfo = 'text', domain = list(x = c(0, 0.25), y = c(0, 0.45))) %>%
                  add_pie(data = visitors_list[[6]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[6]][,1]),hoverinfo = 'text', domain = list(x = c(0.25, 0.50), y = c(0, 0.45))) %>%
                  add_pie(data = visitors_list[[7]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[7]][,1]),hoverinfo = 'text', domain = list(x = c(0.50, 0.75), y = c(0, 0.45))) %>%
                  add_pie(data = visitors_list[[8]], labels = independent_var, values = ~presence, textinfo = 'percent', text = ~paste("origin: ", visitors_list[[8]][,1]),hoverinfo = 'text', domain = list(x = c(0.75, 1), y = c(0, 0.45))) %>%                 

                                   
                  layout(title = title, height = 580, annotations = list(
                                  list(x = 0.10 , y = 1.05, text = provinces[1], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.37 , y = 1.05, text = provinces[2], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.62 , y = 1.05, text = provinces[3], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.90 , y = 1.05, text = provinces[4], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.10 , y = 0.48, text = provinces[5], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.37 , y = 0.48, text = provinces[6], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.62 , y = 0.48, text = provinces[7], showarrow = F, xref='paper', yref='paper'),
                                  list(x = 0.90 , y = 0.48, text = provinces[8], showarrow = F, xref='paper', yref='paper')                                  
                                  )
                   )         

 
                  p
                  
                })
                
   
                
                output$ca_it <- render_province_it("Cagliari", input)
                output$ca_sub_it <- province_curve("Cagliari", input)
                output$ca_st <- render_province_st("Cagliari", input)
                
                
                output$or_it <- render_province_it("Oristano", input)
                output$or_sub_it <- province_curve("Oristano", input)
                output$or_st <- render_province_st("Oristano", input)
                
                output$nu_it <- render_province_it("Nuoro", input)
                output$nu_sub_it <- province_curve("Nuoro", input)
                output$nu_st <- render_province_st("Nuoro", input)
                
                output$ci_it <- render_province_it("Carbonia-Iglesias", input)
                output$ci_sub_it <- province_curve("Carbonia-Iglesias", input)
                output$ci_st <- render_province_st("Carbonia-Iglesias", input)
                
                output$og_it <- render_province_it("Ogliastra", input)
                output$og_sub_it <- province_curve("Ogliastra", input)
                output$og_st <- render_province_st("Ogliastra", input)
                
                output$ot_it <- render_province_it("Olbia-Tempio", input)
                output$ot_sub_it <- province_curve("Olbia-Tempio", input)
                output$ot_st <- render_province_st("Olbia-Tempio", input)
                
                output$ss_it <- render_province_it("Sassari", input)
                output$ss_sub_it <- province_curve("Sassari", input)
                output$ss_st <- render_province_st("Sassari", input)
                
                output$vs_it <- render_province_it("Medio Campidano", input)
                output$vs_sub_it <- province_curve("Medio Campidano", input)
                output$vs_st <- render_province_st("Medio Campidano", input)
                
                output$comparazione <- renderPlotly({
                        vodafone_data <- fread("data/validation/vodafone_global_daily_presence.csv")
                        real_data <- fread("data/validation/global_real_arrivals_departures_partial.csv")
                        sired_data <- fread("data/validation/sired_global_daily_presence.csv")
                        
                        vodafone_validation <- create_vodafone_validation_dataset(vodafone_data)
                        real_validation <- create_real_validation_dataset(real_data)
                        sired_validation <- create_sired_validation_dataset(sired_data)
                        
                        global <- cbind(vodafone_validation, real_validation$daily_difference, sired_validation$daily_difference)
                        names(global)[3:5] = c("Vodafone", "Ports_Airports", "Sired")
                        
                        p <- plot_ly(global, x = ~month, y = ~Vodafone, type = 'bar', name = 'Dati Vodafone', marker = list(color = 'red')) %>%
                                add_trace(y = ~Ports_Airports, name = 'Dati Portuali e Aeroportuali', marker = list(color = 'rgb(0, 153, 0)')) %>%
                                add_trace(y = ~Sired, name = 'Dati Sired', marker = list(color = 'orange')) %>%                                
                                layout(title = "Comparazione presenze nette mensili 2016", yaxis = list(title = 'monthly net presensences'), barmode = 'group')

                })
          
                

                
})        





