REGION_MAPPINGS <- fread("mappings/region_map.csv")
NATION_MAPPINGS <- fread("mappings/nation_map.csv")


filter_municipalities <- function(municipalities){
        
        filtered_municipalities <- sapply(municipalities, function(mun){
                mun = trimws(mun)
                gsub(pattern = "other municipalities", x = mun, replacement = "Others", ignore.case = T)})
        names(filtered_municipalities) = NULL
        filtered_municipalities
        
}

name_adapter <- function(list_to_convert, reg = T){
        
        if(reg == T){
                keys = as.character(REGION_MAPPINGS[["sired_regions"]])
                mapper = as.character(REGION_MAPPINGS[["vodafone_regions"]])
                
        }else{
                keys = as.character(NATION_MAPPINGS[["sired_nations"]])
                mapper = as.character(NATION_MAPPINGS[["vodafone_nations"]])
                
        }
        
        names(mapper) = keys
        res <- as.character(mapper[list_to_convert])
        res

}

