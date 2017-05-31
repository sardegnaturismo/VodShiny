regional_visitors_by_month <- function(dataset){
  ###here we aggregate the visitors to the specified municipality by month
  dataset = dataset[dataset$municipality == municipality_name,]
  res <- aggregate(dataset$unique_visitors ~ dataset$day, FUN = sum)
  names(res) <- c("month", "visitors")
  res <- res[order(as.Date(res$month, format="%d/%m/%Y"), decreasing = F),]
  row.names(res) <- NULL
  res <- cbind(period, res)
  res$period <- factor(res$period, levels = res[["period"]])
  
  res
  
  
  
}