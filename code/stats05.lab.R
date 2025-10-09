pacman::p_load(tidyverse,
               patchwork,
               here)

## visual for PlantGrowth
df_plantgrowth <- tibble(PlantGrowth)

df_plantgrowth %>%
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2,
              fill = "lightblue") +
  geom_jitter(width = 0.1) +
  theme_bw()

## anova of PlantGrowth

m <- aov(weight ~ group,
    data = df_plantgrowth)

summary(m)

##install.packages("pwr")
library(pwr)
## n group = 3
## f = 0.5
## power = 0.8
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

## change, f, k, power, one at a time
## seeing how the changes affect the number of samples needed

# k = 7
pwr::pwr.anova.test(k = 7,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)
# f = 0.2
pwr::pwr.anova.test(k = 3,
                    f = 0.2,
                    sig.level = 0.05,
                    power = 0.8)
# power = 95%
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.95)
# k = 50
pwr::pwr.anova.test(k = 50,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)
# f = 0.9
pwr::pwr.anova.test(k = 3,
                    f = 0.9,
                    sig.level = 0.05,
                    power = 0.8)
# power = 75%
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.75)

## running without power known but n is 
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)
## different levels of k, n, f
# k = 10
pwr::pwr.anova.test(k = 10,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)
# n = 50
pwr::pwr.anova.test(k = 3,
                    n = 50,
                    f = 0.5,
                    sig.level = 0.05)
# f = 0.2
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.2,
                    sig.level = 0.05)
