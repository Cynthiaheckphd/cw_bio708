library(tidyverse)
library(patchwork)
## 8.3.1

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

mu <- mean(df_h0$height)
sigma2 <- sum(((df_h0$height - mu)^2) / nrow(df_h0))

mu_i <- var_i <- NULL 

for (i in 1:100) {
  df_i <- df_h0 %>%
    sample_n(size = 50)
  
  (mu_i[i] <- mean(df_i$height))
  (var_i[i] <- var(df_i$height))
}

df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits= c(18, 22))

g_var <- df_sample %>%
  ggplot(aes(x = var_i)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(15, 40))

(g_mu / g_var)

#100 samples

mu100_i <- var100_i <- NULL 

for (i in 1:100) {
  df100_i <- df_h0 %>%
    sample_n(size = 100)
  
  (mu100_i[i] <- mean(df100_i$height))
  (var100_i[i] <- var(df100_i$height))
}

df100_sample <- tibble(mu_hat = mu100_i,
                    var_hat = var100_i)

g100_mu <- df100_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits= c(18, 22))

g100_var <- df100_sample %>%
  ggplot(aes(x = var100_i)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(15, 40))

(g100_mu / g100_var)

# Compare 100 v 50 samples
(g_mu / g100_mu)
(g_var / g100_var)

## 8.3.2
df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu10 <- mean(df_h10$height)
sigma10 <- sum(((df_h0$height - mu10)^2) / nrow(df_h10))

# 50 samples
mu10_i <- var10_i <- NULL 

for (i in 1:100) {
  df10_i <- df_h10 %>%
    sample_n(size = 50)
  
  (mu10_i[i] <- mean(df10_i$height))
  (var10_i[i] <- var(df10_i$height))
}

df10_sample <- tibble(mu_hat = mu10_i,
                    var_hat = var10_i)

g10_mu <- df10_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu10)

g10_var <- df_sample %>%
  ggplot(aes(x = var_i)) +
  geom_histogram() +
  geom_vline(xintercept = sigma10)

(g10_mu / g10_var)
