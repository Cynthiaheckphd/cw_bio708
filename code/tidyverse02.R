#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# Group Operation ---------------------------------------------------------

# combining group_by with summarize
df_m_sd <- iris_sub %>%
  group_by(Species) %>%
  summarize(mean_sul = mean(Sepal.Length),
            sd = sd(Sepal.Length))

#combining group_by with mutate

df_eps <- iris_sub %>%
  group_by(Species) %>%
  mutate(mean_sl = mean(Sepal.Length)) %>%
  ungroup() %>%
  mutate(eps = abs(Sepal.Length - mean_sl))


