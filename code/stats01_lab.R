library(tidyverse)
# 7.3.1
# Q1 ----------------------------------------------------------------------


z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))
# art mean
mu_z <- mean(z)
# geom mean
g_z <- exp(mean(log(z)))

# median
z <- sort(z)
index <- (length(z) + 1) / 2
z[index]
med_z <- median(z)


# Q2 ----------------------------------------------------------------------
df_z <- tibble(z = z)
df_z %>%
  ggplot(aes(x = z)) +
           geom_histogram()


# Q3 Q4 ----------------------------------------------------------------------

g1 <- df_z %>%
  ggplot(aes(x = z)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z,
             color = "pink") +
  geom_vline(xintercept = g_z,
             color = "green") +
  geom_vline(xintercept = med_z,
             color = "blue")

# Q5 -----------------------------------------------------------------------

## install.packages("patchwork")
z_rev <- -z + max(z) + 0.1
mu_zrev <- mean(z_rev)
g_zrev <- exp(mean(log(z_rev)))
med_zrev <- median(z_rev)

df_zrev <- tibble(z_rev = z_rev)
df_zrev %>%
  ggplot(aes(x = z_rev)) +
  geom_histogram()

g2 <- df_z %>%
  ggplot(aes(x = z_rev)) +
  geom_histogram() +
  geom_vline(xintercept = mu_zrev,
             color = "pink") +
  geom_vline(xintercept = g_zrev,
             color = "green") +
  geom_vline(xintercept = med_zrev,
             color = "blue")

library(patchwork)
g1 / g2

# 7.3.2

# Q1 ----------------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1) #units g
head(w) # show first 10 elements in w

# gram to milligram
m <- 1000 * w # united mg

# Q2 ----------------------------------------------------------------------
# standard deviation
s2_w <- sum((w - mean(w))^2) / length(w)
s_w <- sqrt(s2_w)

s2_m <- sum((m - mean(m))^2) / length(m)
s_m <- sqrt(s2_m)

#MAD
mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

# Q3 ----------------------------------------------------------------------

# CV (makes the data unitless)
cv_w <- s_w / mean(w) #numerator and denominator have same unit [g]
cv_m <- s_m / mean(m)

# MAD / median
madr_w <- mad_w / median(w)
madr_m <- mad_m / median(m)
