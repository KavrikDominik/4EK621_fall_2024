library(dplyr)
library(sandwich)
library(stargazer)
library(tidyr)
library(mfx)
library(pROC)
library(modelr)
library(caret)

# Data --------------------------------------------------------------------
data <- read.csv("week_12/data/scoring.csv")
data <- data[complete.cases(data),] # removes observation with NAs

data <- data %>% 
  mutate(deliq = ifelse(NumberOfTime30.59DaysPastDueNotWorse > 0, 1, 0))

descriptive_statistics <- summary(data)

equation <- as.formula(SeriousDlqin2yrs ~ age + MonthlyIncome + deliq)



# Train/Test split --------------------------------------------------------



# LMP - Linear Probability Model ------------------------------------------



# Logit -------------------------------------------------------------------



# Logit MLE from the scratch ----------------------------------------------

neg_log_likelihood_logistic <- function(beta, X, y) {
  z <- as.matrix(cbind(1, X)) %*% beta  # Calculate linear predictors including intercept
  log_likelihood <- sum(y * z - log(1 + exp(z)))  # Log-likelihood
  return(-log_likelihood)  # Return negative log-likelihood for minimization
}

# Initial guesses for parameters (intercept and slopes)
initial_values <- c("Intercept"=0,
                    "age"=0,
                    "MonthlyIncome"=0,
                    "deliq"=0)  # Intercept and two slopes

X = as.matrix(
        data[,c("age", "MonthlyIncome" ,"deliq")]
    )


# Minimize the negative log-likelihood using 'optim'
opt_result_logistic <- optim(
  initial_values,
  neg_log_likelihood_logistic,
  X=X, 
  y=data$SeriousDlqin2yrs)

# Display the estimated parameters (log-odds)
opt_result_logistic$par
summary(Logit)


# Probit ------------------------------------------------------------------


# Predictions -------------------------------------------------------------

# ROC curve ---------------------------------------------------------------
#The Receiver Operating Characteristic (ROC)

# calculating partial effects ---------------------------------------------
