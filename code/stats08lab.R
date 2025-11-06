pacman::p_load(tidyverse,
               patchwork,
               here)


# glm exercise ------------------------------------------------------------
##fish
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

glm_dist <- glm(n_sp ~ distance,
                data = df_fish,
                family = "poisson")

glm_cat <- glm(n_sp ~ cat_area,
                data = df_fish,
                family = "poisson")

glm_hull <- glm(n_sp ~ hull_area,
                data = df_fish,
                family = "poisson")

glm_fish <- glm(n_sp ~ distance + cat_area + hull_area,
                data = df_fish,
                family = "poisson")

summary(glm_fish)

##mtcars
print(mtcars)

glm_am <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
    data = mtcars,
    family = "binomial")

summary(glm_am)

m_am <- glm(am ~ mpg + hp + wt,
              data = mtcars,
              family = "gaussian")

##scale fish data

std_dist <- scale(df_fish$distance)
std_cat <- scale(df_fish$cat_area)
std_hull <- scale(df_fish$hull_area)

df_fish <- df_fish %>%
  mutate(std_cat = std_cat,
         std_dist = std_dist,
         std_hull = std_hull)

glm_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
    data = df_fish,
    family = "poisson")
summary(glm_std)
summary(glm_fish)

##offset term
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)
print(df_offset)

g1 <- ggplot(df_offset) +
  geom_point(aes( x = nitrate,
                  y = count))

g2 <- ggplot(df_offset) +
  geom_point(aes( x = area,
                  y = count))
df_offset <- df_offset %>%
  mutate(density = count/area)

g3 <- ggplot(df_offset) +
  geom_point(aes( x = nitrate,
                  y = density))

g1 / g2 / g3

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

m_offset1 <- glm(count ~ nitrate,
    data = df_offset,
    family = "poisson")

m_offset2 <- glm(count ~ nitrate + offset(log(area)),
    data = df_offset,
    family = "poisson")

summary(m_offset1)
summary(m_offset2)

##overdispersion
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

print(df_tadpole)

gg1 <- ggplot(df_tadpole) +
  geom_point(aes(x = aqveg,
                 y = tadpole))

gg2 <- ggplot(df_tadpole) +
  geom_point(aes(x = permanence,
                 y = tadpole))
gg1 /gg2

glm_perm <- glm(tadpole ~ permanence, 
    data = df_tadpole,
    family = "poisson")

summary(glm_perm)

glm_veg <- glm(tadpole ~ aqveg,
    data = df_tadpole,
    family = "poisson")
summary(glm_veg)

mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)  ##poisson will only give accurate results is mean and var are close to same

## negative binomial regression

m_nb <- MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole)
summary(m_nb)
