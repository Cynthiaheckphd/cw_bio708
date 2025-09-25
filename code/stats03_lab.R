library(tidyverse)
library(patchwork)


# normal distribution -----------------------------------------------------

v <- rnorm(50, mean = 100, sd = 2)

x_min <- floor(min(v)) 
x_max <- ceiling(max(v))
bin <- seq(x_min, x_max, by = 1)

pnorm(bin[2], mean = mean(v), sd = sd(v)) - pnorm(bin[1], mean = mean(v), sd = sd(v))

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mean(v), sd = sd(v)) - 
    pnorm(bin[i], mean = mean(v), sd = sd(v))
}

df_prob <- tibble(bin = bin[-length(bin)] + 0.5,
       prob = p) %>%
  mutate(freq = p * length(v))

df_v <- tibble(v = v)

df_v %>% 
  ggplot(aes(x = v)) + 
  geom_histogram() +
  geom_point(data = df_prob, 
             aes(x = bin,
                 y = freq)) +
  geom_line(data = df_prob,
            aes(x = bin,
                y = freq))


# poisson distribution ----------------------------------------------------

x <- rpois(n = 1000, lambda = 10)
bin <- seq(0, 10, by = 1)

pm <- dpois(x, lambda = mean(x))

df_prob <- tibble(x = x,
                  y = pm) %>%
  mutate(freq = pm * length(x))

df_x <- tibble(x = x)

df_x %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq)) +
  labs(y = "Probability",
       x = "Count")
