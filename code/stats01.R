# Ctrl + Shift + N to create a new script file

library(tidyverse)

# central tendency ------------------------------------------------------
# hotkey for a section Ctrl + Shift + R

# arithmetic mean
# calculate the arithmetic mean of v_x using length() and sum()
v_x <- rnorm(10)
mu_x <- sum(v_x) / length(v_x)

#geometric mean
# use prod(), length(), ^
v_y <- runif(10, min = 10, max = 20)
prod(v_y)^ (1/length(v_y))

#mean of log = same as geometric mean (geo mean is insensitive to outliers)
exp(mean(log(v_y)))

# median 
v_z <- runif(9, min = 10, max = 20)
v_z <- sort(v_z)
# (N + 1) / 2
index <- (length(v_z) + 1) / 2
v_z[index]
median(v_z)


# variance measures -------------------------------------------------------

# variance 
# use sum(), mean(), length(), and ^
v_a <- rnorm(100)
s2 <- sum((v_a - mean(v_a))^2) / length(v_a)
# standard deviation (= sqrt of variance)
s <- sqrt(s2)

# inter-quantile range
a_l <- quantile(v_a, probs = 0.25)
a_h <- quantile(v_a, probs = 0.75)
(iqr <- abs(a_h - a_l))

# mean absolute deviation (MAD)
median(abs(v_a - median(v_a)))

# coefficient of variation (CoV)
# use s and mean() of v_b to define CV
v_b <- runif(100, min = 10, max = 20)
s2 <- sum((v_b - mean(v_b))^2) / length(v_b)
s <- sqrt(s2)
cv <- s / mean(v_b)

# MAD / median
(median(abs(v_b - median(v_b)))) / median(v_b)
# or 
madv_b <- median(abs(v_b - median(v_b)))
medv_b <- median(v_b)
(mad2med <- madv_b / medv_b)
