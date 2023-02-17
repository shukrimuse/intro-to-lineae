##student test-----

#base R
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

#tidyverse
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

#values for critical t at each degree of freedom up to 30 are presented below
df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

#summary of model
lsmodel1 <- lm(height ~ type, data = darwin)
summary(lsmodel1)
#second row of table tests null hypothesis
#first row of table also performs t-test and tests null hypothesis that intercept is 0

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

##paired t----

#generate paired t test

#base R
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

#tidyverse
darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()
#now the intercept is the height of the crossed plant from pair 1
#the second row now compares the mean heights of Crossed and Selfed plants when they are in the same pair
#rows three to 16 compare the average difference of each pair (Crossed and Selfed combined) against pair 1


#generate the confidence intervals for the paired t-test
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows
#observation: estimate of the mean difference is identical but the 95% confidence intervals are now slightly different

#model graphs
m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

#made a for loop that assumes we 'know' the true mean difference of crossed and fertilised plants and the standard deviation of the 'population'
#loop then creates 20 new sampling experiments, and calculates the estimated mean difference for each experiment
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments


#calculating how many experiments found a significant difference and how many did not
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())
#observation: nearly a third of the experiments did not find a statistically significant difference


#comparing the estimates and confidence intervals
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()
#observations: All 20 studies showed the effect of inbreeding depression, and all the experiments have identical levels of uncertainty
