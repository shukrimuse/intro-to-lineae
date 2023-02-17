library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

#fitting linear model
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

#model summary
summary(lsmodel0)

#overall mean
mean(darwin$height)

#comparing means
lsmodel1 <- lm(height ~ type, data=darwin)
# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

#summarizes information about model components
broom::tidy(lsmodel1)
#analysis shows intercept value has changed and that label of second row is typeSelf

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

#fuller summary of model 
summary(lsmodel1)
#standard error of mean calculated in intercept row- SEM
#standard error of difference between the two means calculated in typeSelf row- SED

#superimpose calculated means onto a plot 
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

#computing confidence intervals
confint(lsmodel1)
broom::tidy(lsmodel1, conf.int=T)

#produces a graph of the estimated mean difference 
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

