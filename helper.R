extract_from_string <- function(concatenated){
  separated_string <- as.list(strsplit(concatenated, ",")[[1]])
  separated_string
}

extract_country_from_string <- function(concatenated){
  separated_string <- extract_from_string(concatenated)
  country <- substring(separated_string[[1]],1)
  country
}

extract_local_info_from_string <- function(concatenated){
  split_list <- extract_from_string(concatenated)
  local_info <- substring(split_list[[2]],2)
  local_info
}

extract_locality_from_string <- function(concatenated){
  local_info <- extract_local_info_from_string(concatenated)
  split_list <- as.list(strsplit(local_info, ":")[[1]])
  locality <- substring(split_list[[1]],1)
  locality
}

extract_ethnic_from_string <- function(concatenated){
  local_info <- extract_local_info_from_string(concatenated)
  split_list <- as.list(strsplit(local_info, ":")[[1]])
  if(length(split_list)>1){
    ethnic <- substring(split_list[[2]],2)
    ethnic
  }
  else{
    NULL
  }
}

extract_country_from_locality <- function(name_of_country){
  country <- sub("(.*?),.*","\\1",name_of_country)
  country
}

extract_code_from_cancer <- function(type_of_cancer){}
