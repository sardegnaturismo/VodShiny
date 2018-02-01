source("R/constants.R")

destination_by_provinces <- function(dataset, province_name){
        d <- dataset[dataset$province == province_name, ]
        res <- aggregate(d$unique_visitors ~ d$origin_province, FUN = sum)
        names(res) <- c("origin", "visitors")
        res <- res[order(res$visitors, decreasing = T), ]
        res$origin <- factor(res$origin, levels = res$origin)
        res
        
}


destination_by_province_and_time <- function(dataset, province_name){
        d <- dataset[dataset$province == province_name, ]
        res <- aggregate(d$unique_visitors ~ d$origin_province + d$day, FUN = sum)
        names(res) <- c("origin_province", "month", "visitors")
        res$month = gsub(pattern = " 0:00", replacement = "", x = res$month)
        res$month <- as.Date(res$month, "%m/%d/%y")
        res = res[order(res$month, decreasing = F), ]
}


get_province_symbols <- function(province_names){
        
        prov_symbols = c()
        for (p in province_names){
                symbol = province_symbols[[p]]
                prov_symbols = append(prov_symbols, symbol)
        }
        return(prov_symbols)
        
}