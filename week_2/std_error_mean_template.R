library(dplyr)

# generate_data
N <- 10e6
y <- rnorm(N, 5, 1) # 5 =true popul. mean
popul <- data.frame(y)
set.seed(123)


# Tasks: ------------------------------------------------------------------

# Sample 100 obs.
# Calculate standard error of mean using formula
# Re-sample and store sample mean of each iteration
# Show results as histogram
# Calculate SEM using resampling

