pacman::p_load(tidyverse,
               patchwork,
               here)

# t test vs lm ------------------------------------------------------------


df_fl <- read.csv(here("data_raw/data_fish_length.csv"))

m <- lm(length ~ lake,
   data = df_fl)

# calculate group means for legth
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length)) %>%
  pull(mu_l)

# mean length for lake a 
v_mu[1]

# mean length for lake b
v_mu[2]

# difference between a and b 
v_mu[2] - v_mu[1]

# mean length for lake b
sum(coef(m))

# looking into lm result
summary(m)

x <- df_fl %>%
  filter(lake == "a") %>%
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)

t.test(x, y, var.equal = TRUE)


# ANOVA vs lm -------------------------------------------------------------

df_anova <- read_csv(here("data_raw/data_fish_length_anova.csv"))
print(df_anova)

m1 <- lm(length ~lake,
   data = df_anova)

v_mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length)) %>%
  pull(mu_l)

summary(m1)

# corresponds to intercept
v_mu_anova[1]

# corresponds to lake c effect
v_mu_anova[3] - v_mu_anova[1]

# corresponds to lake b effect
v_mu_anova[2] - v_mu_anova[1]

aov(length ~ lake,
    data = df_anova) %>%
  summary()

# ancova (Analysis of covarience)----------------------------------------------------------

m2 <- lm(Sepal.Length ~ Sepal.Width + Species,
   data = iris)

summary(m2)

## visualization of ancova example

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
         data = iris)
summary(m_iris)

df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                         max(iris$Petal.Width),
                         length = 100),
                         times = 3),
       Species = rep(unique(iris$Species),
                     each = 100)
       )

# get predicted values
y_pred <- predict(m_iris,
        newdata = df_pred)

df_pred <- df_pred %>%
  mutate(y_pred = y_pred)

ggplot(iris,
       aes(x = Petal.Width,
           y = Petal.Length,
           color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))


# interaction  ------------------------------------------------------------

# how to include interaction 
m_int <- lm(Petal.Length ~ Petal.Width * Species,
   data = iris)

# identical model with different expression
# lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species,
#    data = iris)
