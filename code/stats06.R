pacman::p_load(tidyverse,
               patchwork,
               here)

# read data ---------------------------------------------------------------


df_algae <- read.csv(here("data_raw/data_algae.csv"))


# visualize ---------------------------------------------------------------


df_algae %>%
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()


# try regression ----------------------------------------------------------


(m <- lm(biomass ~ conductivity,
   data = df_algae))
summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>%
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)


# t-value -----------------------------------------------------------------

vcov(m) ##variance co-variance matrix, use to get standard error

diag(vcov(m)) ## variance of estimates

se <- sqrt(diag(vcov(m))) ## standard error

t_value <-beta / se[2] ## t value of beta (slope) coef

## p value of slope
(1 - pt(t_value, df = 48)) + (pt(-t_value, df = 48))


# coefficient of determination --------------------------------------------

##residual 
eps <- resid(m)

df_algae <- df_algae %>%
  mutate(eps = eps)

df_algae %>%
  ggplot(aes(x = conductivity, 
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")

ss <- sum(eps^2)
ss0 <- sum((df_algae$biomass - mean(df_algae$biomass))^2)

## ss /ss0 equals 1 when the model is poor, but ss / ss0 equals 0 when is perfect fit
r_sq <- 1 - (ss / ss0)

## compare with lm output
summary(m)
