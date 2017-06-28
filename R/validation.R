require(dplyr)

create_validation_dataset <- function(dataset, selected_year){
        ## compute daily differences        
        global <- dataset
        global$daily_difference <- c(0, diff(global$residents + global$visitors + global$foreigners))
        ## aggregate daily differences by month
        pivot <- aggregate(data = global, daily_difference ~ month(date) + year(date), FUN = sum)
        names(pivot) <- c("month", "year", "daily_difference")
        ## filter desired values
        if (selected_year == 2016){
                pivot <- filter(pivot, year == selected_year)       
        }
        ## convert month numbers to the corresponding month name
        pivot$month = month.name[pivot$month]
        pivot$month <- factor(pivot$month, levels = pivot$month)                               
        pivot
        
}

create_vodafone_validation_dataset <- function(dataset, selected_year = 2016){
        res <- create_validation_dataset(dataset, selected_year)
        res
}

create_sired_validation_dataset <- function(dataset, selected_year = 2016){
        italians <- aggregate(data = dataset, presenze_italia  ~ giorno + mese + anno, FUN = sum)
        foreigners <- aggregate(data = dataset, presenze_stranieri  ~ giorno + mese + anno, FUN = sum)
        global <- cbind(italians, foreigners$presenze_stranieri)
        ## here we create a dummy variable to be able to use the 
        ## general function create_validation dataset
        global$residents <- 0
        names(global) <- c("giorno", "mese", "anno", "visitors", "foreigners", "residents")
        global$date <- paste(global$anno, global$mese, global$giorno, sep = "/")
        global$date <- as.Date(global$date)
        
        res <- create_validation_dataset(global, selected_year)
        res
}

create_sired_monthly_validation_dataset <- function(dataset){
      res <- aggregate(data = dataset, differenza ~ mese + anno, FUN = sum)
      res$mese <- month.name[res$mese]
      names(res) <- c("month", "year", "daily_difference")
      res
      
}

create_real_validation_dataset <- function(dataset, selected_year = 2016, selected_months = 9){
        date <- paste("01", dataset$mese, dataset$anno, sep = "/")
        dataset$date <- as.Date(date, "%d/%m/%Y")
        dataset$residents <- 0
        
        arrivals <- aggregate(data = dataset, arrivi ~ month(date) + year(date), FUN = sum)
        departures <- aggregate(data = dataset, partenze ~ month(date) + year(date), FUN = sum)
        
        names(arrivals) <- c("month", "year", "arrivals")
        names(departures) <- c("month", "year", "departures")

        global <- cbind(arrivals, departures$departures)
        names(global)[4] = "departures"
        

        global$daily_difference <- global$arrivals - global $departures
        global[1:selected_months, ]        

}



