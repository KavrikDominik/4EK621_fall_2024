set.seed(123)

N <- 1e4
x <- rnorm(N,0,1)
y <- 0.5 + 2.5*x + rnorm(N,0,3)
DF <- tibble(y,x)

DF %>% 
  ggplot(aes(x,y)) +
  geom_point(alpha=0.5, shape=1) +
  theme_light()

DF %>% 
  ggplot(aes(x,y))+
  geom_point(alpha=0.1, shape=1)+
  geom_function(fun= function(x) 0.5+2.5*x, color="blue2", linetype="dashed") + 
  labs(title = "Population regression line") + 
  theme_light()


# Tasks: ------------------------------------------------------------------

# Obtain a sample from the population
# Estimate parameters of simple linear regression model using OLS
# Visualize fitted values
# Show residuals
# Resample multiple times and show parameter distribution
# Introduce a squared term in the simulated population and estimate the tipping point.
                
              
