library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(stringr)
source("week_5/functions/data_functions.R")
options(scipen = 999)


data_folder_path <- "week_5/data/skoda_models"

full_data <- read.csv("week_5/full_data.csv")

colnames(full_data)

regressors <- c("age", "km", "model", "fuel", "displacement", "combi", "gearbox_id")

dependent_variable <- "price"

data <- full_data %>% 
  select(any_of(c( dependent_variable, regressors)))

data_cleaned <- na.omit(data) %>% 
  mutate(
    model = relevel(factor(model), ref = "superb"),
    fuel = relevel(factor(fuel), ref = "petrol"),
    gearbox_id = factor(gearbox_id)
    )

model_1 <- lm(price ~ ., data = data_cleaned)

summary(model_1)

model_restricted <- lm(price ~ age+ km+ model+ fuel+ displacement, data = data_cleaned)


anova(model_1, model_restricted)
unique(data_cleaned$gearbox_id)

data_cleaned <- data_cleaned %>% 
  mutate(age_plus_km = age + km)

model_3 <- lm(price ~ age_plus_km + model+ fuel+ displacement + combi + gearbox_id, data = data_cleaned)

anova(model_1, model_3)
library(car)
linearHypothesis(model_1, c(), test = "F")


model_log <- lm(log(price) ~ age + log(km + 1) + model + fuel + displacement + combi + gearbox_id,
                data = data_cleaned)
summary(model_log)

model_log_sq <-  lm(
  log(price) ~ age + log(km+1) + I(log(km+1)^2)+ model+ fuel+ displacement + combi + gearbox_id,
  data = data_cleaned
  )

0.0994 + 2*(-0.00961)*log(5000)

0.0994 + 2*(-0.00961)*log(200000)

summary(data_cleaned$km)

summary(model_log_sq)
(exp(-.10515)-1)*100
