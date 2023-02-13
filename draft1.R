# Slot machine
# February 13, 2023

library(tidyverse)
wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
payoffs <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' =25,
             'B' = 10, 'C' = 10, '0' = 0)

prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
prob_name <- setNames(prob, wheel)

# Create a matrix of column 3 and n rows of the wheel object
get_symbols <- function(){
    sample(x = wheel, size = 3 * n, replace = TRUE,
           prob = prob)
}


