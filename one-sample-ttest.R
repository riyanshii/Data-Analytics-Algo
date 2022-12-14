set.seed(0)
sweetSold <- c(rnorm(50, mean = 140, sd = 5))
t.test(sweetSold, mu = 150) # Ho: mu = 150