require(dplyr)
#presence_ita <- read.csv("data/sardegna_presence_Sep15-Sep16_Italians_comunes.csv")

get_presence_residents <- function(dataset){
        
        residents <- dataset[dataset$customer_class == "resident", ]
        red_residents <- residents[, c(3,6)]
        
        x <- aggregate(red_residents$presence ~ red_residents$day, data = red_residents, FUN = sum)
        names(x) <- c("date", "presence")
        
        # x1 <- 1:dim(x)[1]
        presence <- as.numeric(x[ , 2])
        # dates <- residents[residents$comune_name == 'Abbasanta', 3]
        date <- as.Date(x$date)
        df <- data.frame(date, presence)
        
}

get_presence_visitors <-  function(dataset){
        visitors <- dataset[dataset$customer_class == "visitor", ]
        red_visitors <- visitors[, c(3,6)]
        x <- aggregate(red_visitors$presence ~ red_visitors$day, data = red_visitors, FUN = sum)
        names(x) <- c("date", "presence")
        
        presence <- as.numeric(x[,2])
        date <- as.Date(x$date)
        df <- data.frame(date, presence)
        
}

get_presence_foreigners <- function(dataset){
        red <- dataset[, c(3,5)]
        x <- aggregate(red$presence_foreigners ~ red$day, data = red, FUN = sum)
        names(x) <- c("date", "presence")
        
        presence <- as.numeric(x[,2])
        date <- as.Date((x$date))
        df <- data.frame(date, presence)
        
        
}

get_tot_visitors_by_prov <- function(dataset, preset = 9){
        visitors <- dataset[dataset$customer_class == "visitor", ]
        red_visitors <- visitors[, c(5,6)]
        x <- aggregate(red_visitors$presence ~ red_visitors$origin, data = red_visitors, FUN = sum)
        names(x) <- c("origin", "presence")
        res <- x[order(x$presence, decreasing = T), ]
        origins <- as.character(res$origin)
        origin <- factor(origins[1:preset], levels = origins[1:preset])
        presence <- res$presence[1:preset]
        res <- data.frame(origin, presence)
        names(res) <- c("origin", "presence")
        res
}

get_tot_visitors_by_prov2 <- function(dataset, perc = 1.0){
      visitors <- dataset[dataset$customer_class == "visitor", ]
      x <- aggregate(data = visitors, presence ~ origin, FUN = sum)
      names(x) <- c("origin", "presence")
      x$perc <- round(100*(x$presence / sum(x$presence)), 2)
      x[x$perc < perc, 1] = "Others"
      res <- aggregate(data = x, presence ~ origin, FUN = sum)
      names(res) <- c("origin", "presence")
      regions <- as.character(res$origin)
      regions[which(regions == 'italian_visitor')] = "Others"
      origin <- factor(regions, levels = regions)
      res$origin = origin
      res <- res[order(res$presence, decreasing = T), ]
      res
  
  
}

get_tot_foreigners_by_prov <- function(dataset, thresh = 0.0){
        red_foreigners = dataset[, c(4,5)]
        x <- aggregate(red_foreigners$presence_foreigners ~ red_foreigners$country, data = red_foreigners, FUN = sum)
        names(x) <- c("country", "presence")
        res <- x[order(x$presence, decreasing = T), ]
        countries <- as.character(res$country)
        country <- factor(countries, levels = countries)
        presence <- res$presence
        res <- data.frame(country, presence)
        names(res) <- c("country", "presence")
        res$perc <- round(100*(res$presence / sum(res$presence)),2)
        
        res2 <- res
        res2[res2$perc < thresh, 1] = "Others"
        res <- aggregate(data = res2, presence ~ country, FUN = sum)
        res <- res[order(res$presence, decreasing = T), ]
        res
}

get_tot_foreigners_by_prov2 <- function(dataset, perc = 1.0){
  x <- aggregate(data = dataset, presence_foreigners ~ country, FUN = sum)
  names(x) <- c("country", "presence")
  x$perc <- round(100*(x$presence / sum(x$presence)), 2)
  x[x$perc < perc, 1] = "Others"
  res <- aggregate(data = x, presence ~ country, FUN = sum)
  names(res) <- c("country", "presence")
  countries <- as.character(res$country)
  country <- factor(countries, levels = countries)
  res$country = country
  res <- res[order(res$presence, decreasing = T), ]
  res
}






