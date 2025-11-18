pacman::p_load(tidyverse,
               patchwork,
               here)

#install.packages("palmerpenguins")
#install.packages("janitor")
library(palmerpenguins,
        janitor)
library(janitor)

print(penguins_raw)


# data manipulation -------------------------------------------------------

colnames(penguins_raw)

##clean column names
##use clean_names()

penguins_clean <- clean_names(penguins_raw)
colnames(penguins_clean)

##change input in clutch_completion
unique(penguins_clean$clutch_completion)

penguins_clean <- penguins_clean %>%
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                    yes = 1,
                                    no = 0))

unique(penguins_clean$clutch_completion)

##change species name input
##use case_when() to change input 
##combine with mutate()
unique(penguins_clean$species)

penguins_clean <- penguins_clean %>%
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo"))

unique(penguins_clean$species)

##remove NA from data 
##drop_na() 

penguins_clean <- penguins_clean %>%
  drop_na(culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g, sex)


# model selection ---------------------------------------------------------
##develop a full model first
penguins_glm <- glm(clutch_completion ~
      species +
      culmen_length_mm +
      culmen_depth_mm +
      flipper_length_mm +
      body_mass_g +
      sex,
    data = penguins_clean,
    family = "binomial")

##model selection with dredge()
library(MuMIn)
options(na.action = "na.fail")
m <- penguins_glm
  m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 2)


