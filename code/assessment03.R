# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- df_co2 %>% 
  rename("plant" = "Plant",
         "type" = "Type",
         "treatment" = "Treatment")

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

df_quebec <- filter(df_co2, type == "Quebec")
df_mississippi <- filter(df_co2, type == "Mississippi")

gg1 <- ggplot(df_quebec,
       aes(x = conc,
           y = uptake,
           color = treatment)) +
  geom_point() +
  labs(title = "Quebec",
       x = "Ambient CO2 Conc",
       y = "CO2 Assimilation Rate")

gg2 <- ggplot(df_mississippi,
                  aes(x = conc,
                      y = uptake,
                      color = treatment)) +
  geom_point() +
  labs(title = "Mississippi",
       x = "Ambient CO2 Conc",
       y = "CO2 Assimilation Rate")

library(patchwork)
gg1 / gg2

# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

m1 <- lm(uptake ~ conc,
   data = df_quebec)
m2 <- lm(uptake ~ treatment,
   df_quebec)
m3 <- lm(uptake ~ conc * treatment,
   data = df_quebec)

m4 <- lm(uptake ~ conc,
   data = df_mississippi)
m5 <- lm(uptake ~ treatment,
         data = df_mississippi)
m6 <- lm(uptake ~ conc * treatment,
   data = df_mississippi)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

coef(m3)
coef(m6)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)
##For Quebec plants: there was significant increase in CO2 assimilation with amb CO2 conc
##but was significantly reduced with chilled treatments (0.022). There was no significant
##effect with treatment(chilled/non) although chilled had a stronger negative effect on uptake.

##For Mississippi plants: am CO2 conc did significantly increase CO2 assimilation.Treatment (chilled/non)
##had significant effect on uptake with chilled treatmed having a more significent negative effect.

## The interaction between conc and treatment reduced CO2 assimilation in both Quebec and
##Mississippi plants as indicated by slopes, however there was only significance of the interaction found within
##Mississippi plants (p value 0.0482 vs Quebec interaction p value 0.852). 

# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- df_env %>% 
  rename("utm.ew" = "UTM.EW",
         "utm.ns" = "UTM.NS",
         "precipitation" = "Precipitation",
         "elevation" = "Elevation",
         "age.cat" = "Age.cat",
         "geology" = "Geology",
         "habitat" = "Habitat",
         "stream" = "Stream",
         "envhet" = "EnvHet")

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

df_env_sub <- df_env %>%
  select(-precipitation, -elevation, - age.cat, - geology)

# Q8
# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.


df_n <- df_bci %>%
  group_by(plot) %>%
  summarize(n_sum = sum(count),
            n1 = max(count),
            p = n1 / n_sum) %>%
  ungroup()

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- left_join(x = df_env_sub,
          y = df_n,
          by = "plot")


# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

var(df_m$p)

glm1 <- glm(formula = cbind(n1, n_sum - n1) ~ envhet + stream + habitat,
    data = df_m,
    family = "binomial")

summary(glm1)

glm2 <- glm(formula = cbind(n1, n_sum - n1) ~ envhet + stream,
            data = df_m,
            family = "binomial")


glm3 <- glm(formula = cbind(n1, n_sum - n1) ~ envhet + habitat,
            data = df_m,
            family = "binomial")


glm4 <- glm(formula = cbind(n1, n_sum - n1) ~ stream + habitat,
            data = df_m,
            family = "binomial")

summary(glm2)
summary(glm3)
summary(glm4)

AIC(glm1, glm2, glm3, glm4)
##variables for the best predictive model are envhet and habitat