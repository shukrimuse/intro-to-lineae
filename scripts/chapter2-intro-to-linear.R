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

#summarizes information about model components
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

#getting the other treatment mean and standard error
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

#emmeans function produces similar result aswell
means <- emmeans::emmeans(lsmodel1, specs = ~ type)
means

#gives a handy summary to include in data visuals that combine raw data and statistical inferences
means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#checking that the residual/unexplained variance in our data is approximately normally distributed
#creating QQ plot, QQ plot is a classic way of checking whether a sample distribution is the same as another
performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

#checking that the residual/unexplained variance is approximately equal between our groups
performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

#check how much of an effect outliers might be having on the model estimates (means)
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

#summary
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

