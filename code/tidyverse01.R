#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# row manipulation exercise -----------------------------------------------

filter(iris_sub, Species == "virginica")
filter(iris_sub, Species %in% c("virginica", "versicolor"))
filter(iris_sub, Species != "virginica")
filter(iris_sub, !(Species %in% c("virginica", "vesicolor")))
filter(iris_sub, Sepal.Length > 5)
filter(iris_sub, Sepal.Length >= 5)
filter(iris_sub, Sepal.Length < 5)
filter(iris_sub, Sepal.Length <= 5)

#Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub, Sepal.Length < 5 & Species == "setosa")
filter(iris_sub, Sepal.Length <5, Species == "setosa")

#Sepal.Length is either less than 5 OR Species equals "setosa"
filter(iris_sub, Sepal.Length <5 | Species == "setosa")

arrange(iris_sub, Sepal.Length)
arrange(iris_sub, desc(Sepal.Length))


# column manipulation exercise --------------------------------------------
select(iris_sub, Sepal.Length)
select(iris_sub, c(Sepal.Length, Sepal.Width))
select(iris_sub, -Sepal.Length)
select(iris_sub, -c(Sepal.Length, Sepal.Width))
select(iris_sub, starts_with ("Sepal"))
select(iris_sub, -starts_with("Sepal"))
select(iris_sub, ends_with("Width"))
select(iris_sub, -ends_with("Width"))

#adding columns
(x_max <- nrow(iris_sub))
x <- x_max
mutate(iris_sub, row.id = x)
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)


# Piping exercise ---------------------------------------------------------

df_vir <- filter(iris_sub, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)

df_vir_sl <- iris_sub %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

print(df_vir_sl)
