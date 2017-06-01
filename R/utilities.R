filter_municipalities <- function(municipalities){
        
        filtered_municipalities <- sapply(municipalities, function(mun){
                mun = trimws(mun)
                gsub(pattern = "other municipalities", x = mun, replacement = "Others", ignore.case = T)})
        names(filtered_municipalities) = NULL
        filtered_municipalities
        
}

name_adapter <- function(ref_list, list_to_change){
      
}