library(dplyr)    # Data manipulation, plotting etc.
library(readr)    # Data manipulation, plotting etc.
library(tidyr)    # Data manipulation, plotting etc.
library(ggplot2)    # Data manipulation, plotting etc.
library(GGally)       # Plot matrix to visualize a dataset quickly
library(skimr)        # Descriptive statistics
# library(texreg)       # Regression tables
library(stargazer)    # Regression tables (alternative)

options(scipen=999)

# Read data ####
skoda <- read_csv("skoda.csv")  # Data on used Skodas





# Tasks:
# Estimate linear regression model using `lm()` function
# Estimate linear regression model using linear algebra
# Calculate ESS, TSS, RSS
# Calculate R^2 
# Calculate adj. R^2


