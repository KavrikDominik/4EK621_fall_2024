library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(stringr)

cleanData <- function(data){
  df <- data %>% 
    mutate(in_operation_date = as.Date(in_operation_date)) %>% 
    pivot_longer(names_to = "fuel",
                 values_to = "values",
                 cols = starts_with("fuel_")) %>% 
    filter(values == TRUE) %>%
    mutate(fuel = stringr::str_remove(fuel, pattern="fuel_")) %>% 
    pivot_wider(names_from = vehicle_attribute,
                values_from = vehicle_attribute_value) %>% 
    mutate(model = as.character(model)) %>% 
    mutate(
      age = difftime(as.Date(Sys.Date()), as.Date(manufacturing_date), units = "days")/365
    ) %>% 
    select(-values)
  return(df)
}

# 
# makeVariables <- function(data){
#   df <- data %>% 
#     mutate(
#     age = difftime(as.Date(Sys.Date()), as.Date(manufacturing_date), units = "days")/365
#   )
# }
# 
