pacman::p_load(tidyverse,
               patchwork,
               here)

# generalized linear model


# count data --------------------------------------------------------------

df_count <- read.csv(here("data_raw/data_garden_count.csv"))

nrow(df_count)

m_normal <- lm(count ~ nitrate,
               data = df_count)

summary(m_normal)

# visualization

alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

ggplot(data = df_count) +
  geom_point((aes(x = nitrate,
                  y = count))) +
  geom_abline(intercept = alpha,
              slope = beta)

## random generator of Poisson distribution
(y <- rpois(n = 10, lambda = 2))

## apply Poisson distribution using glm()
m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

ggplot(df_count) +
  geom_point(aes( x = nitrate,
                  y = count)) +
  geom_abline(intercept = coef(m_pois)[1],
              slope = coef(m_pois)[2])


df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                     max(df_count$nitrate),
                     length = 100))

y_pred <- predict(m_pois,
                  newdata = df_pred) %>%
  exp()

df_pred <- df_pred %>%
  mutate(y = y_pred)

ggplot(df_count,
       aes(x = nitrate,
           y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y))

summary(m_pois)
summary(m_normal)


# binomial modeling -------------------------------------------------------


(df_mussel <- read_csv(here("data_raw/data_mussel.csv")))

## plot data
df_mussel <- df_mussel %>%
  mutate(prop_fert = n_fertilized / n_examined)

ggplot(df_mussel,
       aes(x = density,
           y = prop_fert)) +
  geom_point()

##cbind() is needed for binomial
cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)

## binomial model
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)

## how logit function works
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>%
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line()

