# SET UP ----
## An analysis of the development time of frogspawn in response to water temperature ----

library(tidyverse)
library(rstatix)
library(performance)

#IMPORT DATA
frogs <- read_csv("data/frogs_messy_data.csv") 

#checking data has loaded
head(frogs)

#cleaning data
frogs <- janitor::clean_names(frogs)
colnames(frogs)

frogs <- frogs %>% 
  rename("13" = temperature13,
         "18" = temperature18,
         "25" = temperature25,
         frogspawn_id = `frogspawn_sample_id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)
#___________________________----

#checking data
glimpse(frogs)

#producing a linear model and checking assumptions/interpreting findings 

# ANALYSIS ----
lsmodel_frogs1 <- lm(days ~ temperature, data = frogs)

# summary(lsmodel_frogs1)

# anova(lsmodel_frogs1)

broom::tidy(lsmodel_frogs1, conf.int = T)
#___________________________----

#checking assumptions
plot(lsmodel_frogs1)

performance::check_model(lsmodel_frogs1,
                         check = c("qq", "outliers", "homogeneity"))
