set.seed(0)
shopOne <- rnorm(50, mean = 140, sd = 4.5)
shopTwo <- rnorm(50, mean = 150, sd = 4)
t.test(shopOne, shopTwo, var.equal = TRUE)