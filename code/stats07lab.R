pacman::p_load(tidyverse,
               patchwork,
               here)


# 13.4.1 ------------------------------------------------------------------

## normality assumption

print(ToothGrowth)

m_tooth <- lm(len ~ supp * dose,
   data = ToothGrowth)
summary(m_tooth)

eps <- resid(m_tooth)

shapiro.test(eps)


# 13.4.2 ------------------------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))
y_pred <- predict(m_tooth,
                  newdata = df_pred)
df_pred <- df_pred %>%
  mutate(y_pred = y_pred)

ggplot(data = ToothGrowth,
       aes(x = dose,
           y = len,
           color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))


# 13.4.3 ------------------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))  
ggplot_x1 <- df_y %>%
  ggplot(aes(x = x1,
             y = y)) +
  geom_point(alpha = 0.5)

ggplot_x2 <- df_y %>%
  ggplot(aes(x = x2,
             y =y)) +
  geom_point(alpha = 0.5)

ggplot_x1 / ggplot_x2

m_xy <- lm(y ~ x1 + x2,
   data = df_y)
summary(m_xy)

df_y %>%
  ggplot(aes(x = x1,
             y = x2)) +
  geom_point(alpha = 0.5)

with(df_y,
     cor(x1, x2))
