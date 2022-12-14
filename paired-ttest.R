set.seed(2820)
sweetOne <- c(rnorm(100, mean = 14, sd = 0.3))
sweetTwo <- c(rnorm(100, mean = 13, sd = 0.2))
t.test(sweetOne, sweetTwo, paired = TRUE)