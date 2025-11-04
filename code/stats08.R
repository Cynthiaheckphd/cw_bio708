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
#m_pois <- glm(count ~ nitrate,
#    data = df_count,
#    family = "poisson")

#ggplot(df_count) +
#  geom_point(aes( x = nitrate,
#                  y = count)) +
#  geom_abline(intercept = coef(m_pois)[1],
#              slope = coef(m_pois)[2])


#df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
#                     max(df_count$nitrate),
#                     length = 100))

#y_pred <- predict(m_pois,
#                  newdata = df_pred) %>%
#  exp()

#df_pred <- df_pred %>%
#  mutate(y = y_pred)

#ggplot(df_count,
#       aes(x = nitrate,
#           y = count)) +
#  geom_point() +
#  geom_line(data = df_pred,
#            aes(y = y))

#summary(m_pois)
#summary(m_normal)


(df_mussel <- read_csv(here("data_raw/data_mussel.csv")))

