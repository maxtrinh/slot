# Slot machine
# February 13, 2023

library(tidyverse)
library(glue)
wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
payoffs <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' =25,
             'B' = 10, 'C' = 10, '0' = 0)

prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
prob_name <- setNames(prob, wheel)

# Create a matrix of column 3 of the wheel object
get_symbols <- function(){
    sample(x = wheel, size = 3, replace = TRUE,
           prob = prob)
}

score <- function(symbols){
    # Diamonds ('DD') is a wild card
    
    cherries <- sum(symbols == 'C')
    diamonds <- sum(symbols == 'DD')
    
    # This creates a vector of symbols not equal to DD
    slot <- symbols[symbols != 'DD']
    same <- length(unique(slot)) == 1
    bars <- slot %in% c('BBB', 'BB', 'B')
    
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

play <- function(){
    symbols <- get_symbols()
    score(symbols)
}

winnings <- vector(length = 1000000)
x <- vector(length = 5)

# Takes the average of five trials for more accuracy
for (i in 1:5){
    for (j in 1:length(winnings)) {
        winnings[j] = play()
    }
    x[i] = mean(winnings)
}


mean(winnings) # Expected total win is 93.7 cents out of a dollar

long <- function(){
    for (j in 1:length(winnings)) {
        winnings[j] = play()
    }
}

system.time(long())
#  user    system    elapsed
# 26.091   0.143      26.285 
# First two columns are how many seconds elapsed the user and system execute the process of the call
# Last column is how long R ran the expression

# Play till broke
play_till_broke <- function(starting_cash) {
    
    cash <- starting_cash
    n <- 0
    
    while (cash > 0) {
        cash <- cash - 1 + play()
        n <- n + 1
    }
    n
}

average_broke <- vector(mode = 'numeric', length = 100)
for (i in 1:length(average_broke)){
    average_broke[i] = play_till_broke(100)
}

glue("On average it takes about {round(mean(average_broke),0)} trials to go broke with a starting cash of $100")








