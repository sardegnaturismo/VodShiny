

#Here the reference dataset is 'sardegna_destinations_for_municipalities.csv'
#The function allows to extract the data related to the municipality of interest

period = c("Sept, 15", "Oct, 15", "Nov, 15", "Dic, 15", "Jan, 16", "Feb, 16", "Mar, 16", "Apr, 16", "May, 16", "Jun, 16", "Jul, 16", "Ago, 16", "Sept, 16")

destination_by_municipalities <- function(dataset, municipality_name){

  reference_month <- as.vector(dataset[,1])
  reference_month = unname(sapply(reference_month, function(x){as.numeric(substr(x, start = 1, stop = 1))}))  
  dataset = cbind(reference_month, dataset[, c(2,3,4,5,6)])
  d <- dataset[dataset$municipality == municipality_name, c(1,2,4,5)]
  
  res <- aggregate(d$unique_visitors ~ d$origin_municipality, FUN = sum)
  names(res) <- c("origin", "visitors")
  res <- res[order(res$visitors, decreasing = T), ]
  res
}

destination_by_month <- function(dataset, municipality_name){
        ###here we aggregate the visitors to the specified municipality by month
        dataset = dataset[dataset$municipality == municipality_name,]
        res <- aggregate(dataset$unique_visitors ~ dataset$day, FUN = sum)
        names(res) <- c("month", "visitors")
        res$month <- as.Date(res$month, "%m/%d/%Y")
        res <- res[order(res$month, decreasing = F),]
        row.names(res) <- NULL
        res$month = format(res$month, format = "%b %Y")
        #res <- cbind(period, res)
        res$month <- factor(res$month, levels = res[["month"]])
        names(res) <- c("period", "visitors")
        
        res
}