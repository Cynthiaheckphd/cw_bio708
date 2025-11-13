pacman::p_load(tidyverse,
               patchwork,
               here)

# R squared  --------------------------------------------------------------

set.seed(1) # for reproducibility

# hypothetical sample size
n <- 100

# true intercept and slope
b <- c(0.1, 0.5)

# hypothetical explanatory variable
x1 <- rnorm(n = n, mean = 0, sd = 1)

# create a design matrix
X <- model.matrix(~x1)

# expected values of y is a function of x
# %*% means matrix multiplication
# y = X %*% b equals y = b[1] + b[2] * x
# recall linear algebra
y_hat <- X %*% b

# add normal errors
y <- rnorm(n = n, mean = y_hat, sd = 0.5)

# plot
df0 <- tibble(y = y, x1 = x1)

df0 %>% 
  ggplot(aes(y = y,
             x = x1)) + 
  geom_point()

## develop a linear model
m1 <- lm(y ~ x1,
         data = df0)
summary(m1)

## add random numbers under x2
df0 <- df0 %>%
  mutate(x2 = rnorm(nrow(.)))

df0 %>% 
  ggplot(aes(y = y,
             x = x2)) + 
  geom_point()

m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)
summary(m1)

## model comparison
m1 <- lm(y ~ x1,
         data = df0) %>%
  summary()

m2 <- lm(y ~ x1 + x2, data = df0) %>%
  summary()

m3 <- lm(y ~ x2,
         data = df0) %>%
  summary()
