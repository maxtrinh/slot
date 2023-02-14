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
    
mat$probability = prob_name[mat$w1] * prob_name[mat$w2]* prob_name[mat$w3]

score <- function(symbols){
    # Diamonds ('DD') is a wild card
    
    cherries <- sum(symbols == 'C')
    diamonds <- sum(symbols == 'DD')
    
    # This creates a vector of symbols not equal to DD
    slot <- symbols[symbols != 'DD']
    same <- length(unique(slot)) == 1
    bars <- slot %in% c('BBB', 'BB', 'B')
    
    # prize <- case_when(
    #     diamonds == 3 ~ 100,
    #     same == TRUE ~ unname(payoffs[slot[1]]),
    #     all(bars) == TRUE ~ 5,
    #     cherries > 0 ~ c(0, 2, 5)[cherries + diamonds + 1],
    #     cherries == 0 ~ 0
    # )
    # 
    # prize * 2^diamonds

    if (diamonds == 3) {
        prize = 100
    } else if (same){
        prize <- unname(payoffs[slot[1]])
        # Any combination of bars is 5
    } else if (all(bars)) {
        prize <- 5
    }
    # One C is 2, Two C is 5
    else if (cherries > 0) {
        prize <- c(0, 2, 5)[cherries + diamonds + 1]
    }
    else {
        prize <- 0
    }
    prize * 2^diamonds

}

# Create a for loop to calculate the prize vector in mat
mat$prize <- vector(mode = "numeric", length = nrow(mat))
for (i in 1:nrow(mat)){
    symbols <- unlist(c(mat[i,1], mat[i, 2], mat[i,3]))
    mat$prize[i] <- score(symbols)
    }

long <- function() {
    for (i in 1:nrow(mat)){
    symbols <- unlist(c(mat[i,1], mat[i, 2], mat[i,3]))
    mat$prize[i] <- score(symbols)}
}

system.time(long())

sum(mat$probability*mat$prize)
# Using Case_when
# user      system  elapsed 
# 15.015   0.864    15.902 

# Using Case_when
# user      system  elapsed 
# 0.169     0.003   0.172 



long()







