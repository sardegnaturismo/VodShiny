require(dplyr)
get_covisits <- function(dataset, cust_class = NULL, chosen_localities){
        
        
       if (!is.null(cust_class)){
                dataset = filter(dataset, customer_class == cust_class)
                }
        covisits <- aggregate(data = dataset, unique_custs ~ origin + destination, FUN = sum)   

        tmp <- filter(covisits, origin %in% chosen_localities & destination %in% chosen_localities)
        reference = chosen_localities
        
        res = matrix(0, nrow = length(chosen_localities), ncol = length(chosen_localities))
        # res <- data.frame(m)
        colnames(res) = chosen_localities
        row.names(res) = chosen_localities
        
        for (r in chosen_localities){
                for(c in chosen_localities){
                        value = as.integer(select(filter(tmp, origin == r & destination == c), unique_custs))
                        if (is.na(value)){
                                value = 0
                        }
                        r_idx = which(chosen_localities == r)
                        c_idx = which(chosen_localities == c)
                        res[r_idx, c_idx] = value
                }
        }
        
        chosen_localities <- adapt_local_name(chosen_localities)
        colnames(res) = chosen_localities
        row.names(res) = chosen_localities
        res
        
        
        
        
}

adapt_local_name <- function(localities){
  
    result <- sapply(localities, FUN = function(x) {
      if (tolower(x) != 'porto_torres'){
        gsub("_", " di ", x)
      }else{
        gsub("_", " ", x)
      }
      
    })
  
}