library(tidyverse)
library(rstatix)
library(performance)

#IMPORT DATA
janka <- read_csv("data/janka.csv") 

#checking data has loaded
head(janka)

#cleaning data
janka <- janitor::clean_names(janka)
colnames(janka)

#checking data
glimpse(janka)

#making a plot to assess linear association 
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()

#generating pearson's R
# cor() does not have a data option so need to use the with() function
with(janka, cor(dens, hardness))

janka_ls1 <- lm(hardness ~ dens, data = janka) 
# specify linear model method for line fitting
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

#the results show The blue line represents the regression line, and the shaded interval is the 95% confidence interval band

summary(janka_ls1)

#centering the vales of x and fitting new linear model
dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#producing upper and lower bands of confidence intervals
confint(janka_ls1)
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)
#results show minimum effect size (at 95% confidence) of density on the janka scale is 52.9%

#finding r^2
summary(janka_ls1)
janka_ls1 %>% 
  broom::glance()

#assumptions in regression models
predict(janka_ls1)

resid(janka_ls1)

janka_ls1 %>% 
  broom::augment() %>% 
  head()

#plotting graph with black fitted regression line and red dashed lines representing the residuals
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")


# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

#creating same graph but using less repititive code
model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

#normal distribution 
plot(janka_ls1, which=c(2,2))
performance::check_model(janka_ls1, check=c("normality","qq"))

#equal variance
plot(janka_ls1, which=c(1,3))
performance::check_model(janka_ls1, check="homogeneity")

#looking for potential outliers
plot(janka_ls1, which=c(4,5))
performance::check_model(janka_ls1, check="outliers")
#outlier is 32

#estimates of intercepts and slope 
coef(janka_ls1)
#if new wood sample has density of 65, can calculate the hardness using regression linear equation y= a + bx
#a=intercept and b=dens
# a + bx

-1160.49970 + 57.50667 * 65

#use the coefficients of the model directly to calculate hardness instead
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65

#other ways of predicting values 1 base R
predict(janka_ls1, newdata=list(dens=c(22,35,65)))
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#other ways of predicting values 2 tidyverse
broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65)))
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#using emmeans package t produce quick predictions 
emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

#plotting three new predictied values onto existing figure
pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))

