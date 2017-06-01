


get_sired_visitors_it <- function(dataset, perc = 1.0){

    #compattiano le colonne Bolzano e Trento in Trentino
    Trentino <- dataset[,5] + dataset[,6]
    tmp <- subset(dataset, select = -c(Bolzano, Trento))
    d1 <- subset(tmp, select = names(tmp)[1:4])
    d2 <- subset(tmp, select = names(tmp)[5:ncol(tmp)])
    provenienze <- cbind(cbind(d1, Trentino), d2)
    
    regions <- as.character(names(provenienze)[2:(ncol(provenienze)-1)])
    presence <- as.numeric(provenienze[nrow(provenienze), 2:(ncol(provenienze)-1)])
    res <- data.frame(regions, presence)
    res$regions <- regions
    res$perc <- round(100*(res$presence / sum(res$presence)), 2)
    res[res$perc < perc, 1] = "Others"
    # print(res)
    res <- aggregate(data = res, presence ~ regions, FUN = sum)
    res <- res[order(res$presence, decreasing = T), ]

    res
  
}

get_sired_visitors_st <- function(dataset, perc){
   
    strangers <- dataset[nrow(dataset), 2:ncol(dataset)]
    nations <- names(strangers)
    presence <- as.numeric(strangers)
    
    res <- data.frame(nations, presence)
    res$nations <- as.character(res$nations)
    res$perc <- round(100*(res$presence / sum(res$presence)), 2)
    res[res$perc < perc, 1] = "Others"
    res <- aggregate(data = res, presence ~ nations, FUN = sum)
    res <- res[order(res$presence, decreasing = T), ]
    res
  
  
}