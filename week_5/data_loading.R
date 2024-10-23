library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(stringr)
source("week_5/functions/data_functions.R")

data_folder_path <- "week_5/data/skoda_models"

model_vec <- list.files(data_folder_path)

full_list <- list()

for (mod in model_vec){
  
  full_list[[mod]] <- cleanData(read_csv(
    file.path(data_folder_path, mod),
    locale = locale(encoding = "UTF-16")
  ))
  
}

options(scipen = 999)


full_data <- bind_rows(full_list)

# write.csv(full_data, row.names = FALSE, file = "week_5/full_data.csv")


full_data %>% 
  group_by(model) %>% 
  summarise(min_price = min(price),
            max_price = max(price),
            mean_price = mean(price))
