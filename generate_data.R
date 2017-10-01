library(tidyverse)

n <- 10
beta0 <- -1.6
beta1 <- 0.03
x <- runif(n=n, min=18, max=60)
pi_x <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))
y <- rbinom(n=length(x), size=1, prob=pi_x)
data <- data.frame(x, pi_x, y)
names(data) <- c("age", "pi", "y")
print(data)

# This will generate a list of faculties -- do it with replacement.
sample(c("Arts", "Science", "Business", "Fine Arts", "Health"), size = 10, replace = TRUE,
       prob=c(0.35, 0.25, 0.15, 0.10, 0.15))

