# This file is used to speed up the process to estimate the expected value of slot machine which stands at 93% and took about 26 seconds for a million trials.

library(tidyverse)
wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
payoffs <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' =25,
             'B' = 10, 'C' = 10, '0' = 0)

prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
prob_name <- setNames(prob, wheel)

# set up the grid, 343 rows = 7^3
# mat$prob1 = mat$w1
mat <- expand_grid(w1 = wheel, w2 = wheel, w3 = wheel)  
    
mat$probability = prob_name[mat$w1] * prob_name[mat$w2]             * prob_name[mat$w3]



