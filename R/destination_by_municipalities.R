#Here the reference dataset is 'sardegna_destinations_for_municipalities.csv'
#The function allows to extract the data related to the municipality of interest

destination_by_municipalities <- function(dataset, municipality_name){

  reference_month <- as.vector(dataset[,1])
  reference_month = unname(sapply(reference_month, function(x){as.numeric(substr(x, start = 1, stop = 1))}))  
  dataset = cbind(reference_month, dataset[, c(2,3,4,5,6)])
  d <- dataset[dataset$municipality == municipality_name, c(1,2,4,5)]
  
  res <- aggregate(d$unique_visitors ~ d$origin_municipality, FUN = sum)
  names(res) <- c("origin", "visitors")
  res

}