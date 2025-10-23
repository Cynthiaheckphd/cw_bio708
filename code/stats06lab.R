pacman::p_load(tidyverse,
               patchwork,
               here)
library(dplyr)


# 12.4.1 ------------------------------------------------------------------
iris
# exercise 1
## perform regression models for each species separately
df_setosa <- filter(iris, Species == "setosa")

df_versicolor <- filter(iris, Species == "versicolor")

df_virginica <- filter(iris, Species == "virginica")

lm(Sepal.Width ~ Petal.Width,
   data = df_setosa)

lm(Sepal.Width ~ Petal.Width,
   data = df_versicolor)

lm(Sepal.Width ~ Petal.Width,
   data = df_virginica)


# 12.4.2 ------------------------------------------------------------------
# exercise 2
## model 1: Petal.Width only
m_pw <- lm(Sepal.Width ~ Petal.Width,
   data = df_setosa)
summary(m_pw) # R^2 = 0.05
## model 2: Petal.With and Petal.Length
m_pwpl <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data = df_setosa)
summary(m_pwpl) # R^2 = 0.06
## regression estimate of model 1 = 3.22
## regression estimate of model 2 = 2.89
## R^2 value of model 1 < model 2

# exercise 3
x <- rnorm(nrow(iris), mean = 0, sd = 1)
iris <- iris %>%
  mutate(x =x)

df_ver <- iris %>%
  filter(Species == "versicolor")

m_ver <- lm(Sepal.Width ~ Petal.Width,
               data = df_versicolor)

m_ver_x <- lm(Sepal.Width ~ Petal.Width + x,
                 df_ver)
