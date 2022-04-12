

#Bases on Balls or Stolen Bases

library(Lahman)

library(tidyverse)

library(dslabs)

ds_theme_set()

view(Teams)



#Scatterplot of the relationship between HRs and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  
  ggplot(aes(HR_per_game, R_per_game)) +
  
  geom_point(alpha = 0.5)



#Scatterplot of the relationship between stolen bases and wins

Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  
  ggplot(aes(SB_per_game, R_per_game)) +
  
  geom_point(alpha = 0.5)



#Scatterplot of the relationship between bases on balls and runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  
  ggplot(aes(BB_per_game, R_per_game)) +
  
  geom_point(alpha = 0.5)



#Question 4/6

#Scatterplot of the relationship between at-bats and runs

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  
  ggplot(aes(AB_per_game, R_per_game)) +
  
  geom_point(alpha = 0.5)



#Question 5

?Teams



#Question 7

Tms <- Teams %>% filter(yearID %in% 1961:2001 )

Tms %>% mutate(Win_per_game = W/G, FieldError_per_game = E/G) %>%
  
  ggplot(aes(Win_per_game, FieldError_per_game)) +
  
  geom_point(alpha = 0.5)



#Question 8

Tms %>% mutate(Triples_per_game = X3B/G, Doubles_per_game = X2B/G) %>%
  
  ggplot(aes(Doubles_per_game, Triples_per_game)) +
  
  geom_point(alpha = 0.5)





#Correlation

# create the dataset

library(tidyverse)

library(HistData)

data("GaltonFamilies")

set.seed(1983)

galton_heights <- GaltonFamilies %>%
  
  filter(gender == "male") %>%
  
  group_by(family) %>%
  
  sample_n(1) %>%
  
  ungroup() %>%
  
  select(father, childHeight) %>%
  
  rename(son = childHeight)



#Code im video:

galton_heights <- GaltonFamilies %>%
  
  filter(childNum ==1 & gender == "male") %>%
  
  select(father, childHeight) %>%
  
  rename(son = childHeight)



# means and standard deviations

galton_heights %>%
  
  summarize(mean(father), sd(father), mean(son), sd(son))



# scatterplot of father and son heights

galton_heights %>%
  
  ggplot(aes(father, son)) +
  
  geom_point(alpha = 0.5)





#correlation coefficient

#rho <- mean(scale(x)*scale(y)) ???

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)





#sample correlation is a random variable

# compute sample correlation

R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  
  summarize(r = cor(father, son))

R



# Monte Carlo simulation to show distribution of sample correlation

B <- 1000

N <- 25

R <- replicate(B, {
  
  sample_n(galton_heights, N, replace = TRUE) %>%
    
    summarize(r = cor(father, son)) %>%
    
    pull(r)
  
})

qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))



# expected value and standard error

mean(R)

sd(R)



# QQ-plot to evaluate whether N is large enough

data.frame(R) %>%
  
  ggplot(aes(sample = R)) +
  
  stat_qq() +
  
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))



#Question 4

B <- 1000

N <- 50

R <- replicate(B, {
  
  sample_n(galton_heights, N, replace = TRUE) %>%
    
    summarize(r = cor(father, son)) %>%
    
    pull(r)
  
})

mean(R)

sd(R)



#Question 7

library(Lahman)

Mannschaft <- Teams %>% filter(yearID %in% 1961:2001)

Mannschaft %>% summarise(r = cor(R/G, AB/G)) %>% pull(r) 



#Question 8

Mannschaft %>% summarise(r = cor(W/G, E/G)) %>% pull(r) 



#Question 9

Mannschaft %>% summarise(r = cor(X2B/G, X3B/G)) %>% pull(r) 





#Anscombe's Quartet/Stratification

# number of fathers with height 72 or 72.5 inches

sum(galton_heights$father == 72)

sum(galton_heights$father == 72.5)



# predicted height of a son with a 72 inch tall father

conditional_avg <- galton_heights %>%
  
  filter(round(father) == 72) %>%
  
  summarize(avg = mean(son)) %>%
  
  pull(avg)

conditional_avg



# stratify fathers' heights to make a boxplot of son heights

temp <- galton_heights %>% mutate(father_strata = factor(round(father)))

temp %>%
  
  ggplot(aes(father_strata, son)) +
  
  geom_boxplot() +
  
  geom_point()



# center of each boxplot

galton_heights %>%
  
  mutate(father = round(father)) %>%
  
  group_by(father) %>%
  
  summarize(son_conditional_avg = mean(son)) %>%
  
  ggplot(aes(father, son_conditional_avg)) +
  
  geom_point()



# calculate values to plot regression line on original data

mu_x <- mean(galton_heights$father)

mu_y <- mean(galton_heights$son)

s_x <- sd(galton_heights$father)

s_y <- sd(galton_heights$son)

r <- cor(galton_heights$father, galton_heights$son)

m <- r * s_y/s_x

b <- mu_y - m*mu_x



# add regression line to plot

galton_heights %>%
  
  ggplot(aes(father, son)) +
  
  geom_point(alpha = 0.5) +
  
  geom_abline(intercept = b, slope = m)



#Bivariate normal distribution

galton_heights %>%
  
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  
  filter(z_father %in% -2:2) %>%
  
  ggplot() + 
  
  stat_qq(aes(sample = son)) +
  
  facet_wrap( ~ z_father)



#There are Two Regression Lines

# compute a regression line to predict the son's height from the father's height

mu_x <- mean(galton_heights$father)

mu_y <- mean(galton_heights$son)

s_x <- sd(galton_heights$father)

s_y <- sd(galton_heights$son)

r <- cor(galton_heights$father, galton_heights$son)

m_1 <-  r * s_y / s_x

b_1 <- mu_y - m_1*mu_x



# compute a regression line to predict the father's height from the son's height

m_2 <-  r * s_x / s_y

b_2 <- mu_x - m_2*mu_y



#Question 7

s_father <- 2

s_son <- 3

r <- 0.5

slope_m <- r*s_son / s_father



#Assessment part 2

#set.seed(1989) #if you are using R 3.5 or earlier

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

library(HistData)

data("GaltonFamilies")



female_heights <- GaltonFamilies%>%    
  
  filter(gender == "female") %>%    
  
  group_by(family) %>%    
  
  sample_n(1) %>%    
  
  ungroup() %>%    
  
  select(mother, childHeight) %>%    
  
  rename(daughter = childHeight)



#Question 8

female_heights %>%
  
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))

female_heights %>% summarise(cor(mother, daughter))



#slope regression line predicting daughters heigh given mothers heights

slope <- cor(female_heights$mother, female_heights$daughter) *
  
  sd(female_heights$daughter) / sd(female_heights$mother)

#intercept

intercept <- mean(female_heights$daughter) - slope * mean(female_heights$mother)



#Question 10

(1-slope^2)

slope^2



#Question 11

intercept + slope*60



female_heights %>%
  
  ggplot(aes(mother, daughter)) +
  
  geom_point(alpha = 0.5) +
  
  geom_abline(intercept = intercept, slope = slope)





#Confounding: Are BBs More Predictive?

# find regression line for predicting runs from BBs

library(tidyverse)

library(Lahman)

bb_slope <- Teams %>%
  
  filter(yearID %in% 1961:2001 ) %>%
  
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  
  lm(R_per_game ~ BB_per_game, data = .) %>%
  
  .$coef %>%
  
  .[2]

bb_slope



Modell <- Teams %>%
  
  filter(yearID %in% 1961:2001 ) %>%
  
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  
  lm(R_per_game ~ BB_per_game, data = .)

Modell$coefficients



# compute regression line for predicting runs from singles

singles_slope <- Teams %>%
  
  filter(yearID %in% 1961:2001 ) %>%
  
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  
  .$coef  %>%
  
  .[2]

singles_slope



# calculate correlation between HR, BB and singles

Teams %>%
  
  filter(yearID %in% 1961:2001 ) %>%
  
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>% 
  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))



# find regression line for predicting runs from BBs

library(tidyverse)

library(Lahman)

get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)



bb_slope <- Teams %>%
  
  filter(yearID %in% 1961:2001) %>%
  
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  
  summarize(slope = get_slope(BB_per_game, R_per_game))



bb_slope



# compute regression line for predicting runs from singles

singles_slope <- Teams %>%
  
  filter(yearID %in% 1961:2001) %>%
  
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  
  summarize(slope = get_slope(Singles_per_game, R_per_game))



singles_slope



# calculate correlation between HR, BB and singles

Teams %>%
  
  filter(yearID %in% 1961:2001 ) %>%
  
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>% 
  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))





#Stratification and Multivariate Regression

# stratify HR per game to nearest 10, filter out strata with few points

library(tidyverse)

library(Lahman)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(HR_strata = round(HR/G, 1),
         
         BB_per_game = BB / G,
         
         R_per_game = R / G) %>%
  
  filter(HR_strata >= 0.4 & HR_strata <=1.2)



# scatterplot for each HR stratum

dat %>%
  
  ggplot(aes(BB_per_game, R_per_game)) + 
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm") +
  
  facet_wrap( ~ HR_strata)



# calculate slope of regression line after stratifying by HR

dat %>% 
  
  group_by(HR_strata) %>%
  
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))



# stratify by BB

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(BB_strata = round(BB/G, 1),
         
         HR_per_game = HR / G,
         
         R_per_game = R / G) %>%
  
  filter(BB_strata >= 2.8 & BB_strata <=3.9)



# scatterplot for each BB stratum

dat %>% ggplot(aes(HR_per_game, R_per_game)) + 
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm") +
  
  facet_wrap( ~ BB_strata)



# slope of regression line after stratifying by BB

dat %>% 
  
  group_by(BB_strata) %>%
  
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))





#Least Squares Estimates (LSE)

# compute RSS (residual sum of squares) for any pair of beta0 and beta1 in Galton's data

library(HistData)

data("GaltonFamilies")

set.seed(1983)

galton_heights <- GaltonFamilies %>%
  
  filter(gender == "male") %>%
  
  group_by(family) %>%
  
  sample_n(1) %>%
  
  ungroup() %>%
  
  select(father, childHeight) %>%
  
  rename(son = childHeight)

rss <- function(beta0, beta1){
  
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  
  return(sum(resid^2))
  
}



# plot RSS as a function of beta1 when beta0=25

beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1,
                      
                      rss = sapply(beta1, rss, beta0 = 25))

results %>% ggplot(aes(beta1, rss)) + geom_line()



#The lm function

# fit regression line to predict son's height from father's height

fit <- lm(son ~ father, data = galton_heights)

fit



# summary statistics

summary(fit)





#LSE are random variables

# Monte Carlo simulation

B <- 1000

N <- 50

lse <- replicate(B, {
  
  sample_n(galton_heights, N, replace = TRUE) %>%
    
    lm(son ~ father, data = .) %>%
    
    .$coef
  
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])



# Plot the distribution of beta_0 and beta_1

library(gridExtra)

p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")

p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black")

grid.arrange(p1, p2, ncol = 2)



# summary statistics

sample_n(galton_heights, N, replace = TRUE) %>%
  
  lm(son ~ father, data = .) %>%
  
  summary %>%
  
  .$coef



lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))



#Advanced Note on LSE

lse %>% summarize(cor(beta_0, beta_1))

#with standardized father heights no correlation

B <- 1000

N <- 50

lse <- replicate(B, {
  
  sample_n(galton_heights, N, replace = TRUE) %>%
    
    mutate(father = father - mean(father)) %>%
    
    lm(son ~ father, data = .) %>% .$coef
  
})



cor(lse[1,], lse[2,])



#Predicted variables are random variables

# plot predictions and confidence intervals

galton_heights %>% ggplot(aes(father, son)) +
  
  geom_point() +
  
  geom_smooth(method = "lm")



# predict Y directly

fit <- galton_heights %>% lm(son ~ father, data = .)

Y_hat <- predict(fit, se.fit = TRUE)

names(Y_hat)



# plot best fit line

galton_heights %>%
  
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  
  ggplot(aes(father, Y_hat))+
  
  geom_line()



#Question 1

# plot RSS as a function of beta1 when beta0=25

beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1,
                      
                      rss = sapply(beta1, rss, beta0 = 36))

results %>% ggplot(aes(beta1, rss)) + geom_line()



#Question 3

library(Lahman)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(HR_per_game = HR / G,
         
         BB_per_game = BB / G,
         
         R_per_game = R / G)

dat %>% lm(R_per_game ~ BB_per_game + HR_per_game, data=.)



#Question 5

galton_heights %>% ggplot(aes(father, son)) +
  
  geom_point() +
  
  geom_smooth(method = "lm")





model <- lm(son ~ father, data = galton_heights)

predictions <- predict(model, interval = c("confidence"), level = 0.95)

data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)



ggplot(data, aes(x = father, y = fit)) +
  
  geom_line(color = "blue", size = 1) +
  
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  
  geom_point(data = galton_heights, aes(x = father, y = son))





#Assessment: Least Squares Estimates, part 2

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

library(HistData)

data("GaltonFamilies")

options(digits = 3)    # report 3 significant digits



female_heights <- GaltonFamilies %>%    
  
  filter(gender == "female") %>%    
  
  group_by(family) %>%    
  
  sample_n(1) %>%    
  
  ungroup() %>%    
  
  select(mother, childHeight) %>%    
  
  rename(daughter = childHeight)



#Question 7

Modell <- lm(mother ~ daughter, data=female_heights)

Modell



#Question 8

Vorhersage <- predict(Modell)

Vorhersage[1]

female_heights$mother[1]



#Question 9

library(Lahman)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  
  filter(pa >= 100) %>%
  
  select(playerID, singles, bb)



bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  
  filter(pa >= 100) %>%
  
  select(playerID, singles, bb)



bat_01_means <- bat_01 %>% group_by(playerID) %>%
  
  summarise(mean_singles = mean(singles), mean_bb = mean(bb)) %>% ungroup()

bat_01_means %>% filter(mean_singles > 0.2) %>% summarise(Anzahl = n())

bat_01_means %>% filter(mean_bb > 0.2) %>% summarise(Anzahl = n())

sum(bat_01_means$mean_bb > 0.2)



#Question 10

bat_join <- inner_join(bat_02, bat_01_means)

cor(bat_join$singles, bat_join$mean_singles)

cor(bat_join$bb, bat_join$mean_bb)



#Question 11

bat_join %>% ggplot(aes(singles, mean_singles)) + geom_point()

bat_join %>% ggplot(aes(bb, mean_bb)) + geom_point()



#Question 12

lm(singles ~ mean_singles, data= bat_join)

lm(bb ~ mean_bb, data= bat_join)





#Advanced dplyr: Tibbles

# stratify by HR

library(Lahman)

library(tidyverse)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(HR = round(HR/G, 1),
         
         BB = BB/G,
         
         R = R/G) %>%
  
  select(HR, BB, R) %>%
  
  filter(HR >= 0.4 & HR<=1.2)



# calculate slope of regression lines to predict runs by BB in different HR strata

dat %>% 
  
  group_by(HR) %>%
  
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))



# use lm to get estimated slopes - lm does not work with grouped tibbles

dat %>% 
  
  group_by(HR) %>%
  
  lm(R ~ BB, data = .) %>%
  
  .$coef



# inspect a grouped tibble

dat %>% group_by(HR) %>% head()

dat %>% group_by(HR) %>% class()



#Tibbles: Differences from Data Frames

# inspect data frame and tibble

Teams

as_tibble(Teams)

# Note that the function was formerly called as.tibble()



# subsetting a data frame sometimes generates vectors

class(Teams[,20])



# subsetting a tibble always generates tibbles

class(as_tibble(Teams[,20]))



# pulling a vector out of a tibble

class(as_tibble(Teams)$HR)



# access a non-existing column in a data frame or a tibble

Teams$hr

as_tibble(Teams)$hr



# create a tibble with complex objects

tibble(id = c(1, 2, 3), func = c(mean, median, sd))





#do()

# use do to fit a regression line to each HR stratum

dat %>% 
  
  group_by(HR) %>%
  
  do(fit = lm(R ~ BB, data = .))



# using do without a column name gives an error

dat %>%
  
  group_by(HR) %>%
  
  do(lm(R ~ BB, data = .))



# define a function to extract slope from lm

get_slope <- function(data){
  
  fit <- lm(R ~ BB, data = data)
  
  data.frame(slope = fit$coefficients[2],
             
             se = summary(fit)$coefficient[2,2])
  
}



# return the desired data frame

dat %>% 
  
  group_by(HR) %>%
  
  do(get_slope(.))



# not the desired output: a column containing data frames

dat %>% 
  
  group_by(HR) %>%
  
  do(slope = get_slope(.))



fit <- lm(R ~ BB, data = dat)

temp <- summary(fit)

names(fit$coefficients)

fit$coefficients

summary(fit)$coefficient[,2]



# data frames with multiple rows will be concatenated appropriately

get_lse <- function(data){
  
  fit <- lm(R ~ BB, data = data)
  
  data.frame(term = names(fit$coefficients),
             
             estimate = fit$coefficients,
             
             se = summary(fit)$coefficient[,2])
  
}



temp <- dat %>% 
  
  group_by(HR) %>%
  
  do(get_lse(.))





#broom

# use tidy to return lm estimates and related information as a data frame

library(broom)

fit <- lm(R ~ BB, data = dat)

tidy(fit)



# add confidence intervals with tidy

tidy(fit, conf.int = TRUE)



# pipeline with lm, do, tidy

dat %>% 
  
  group_by(HR) %>%
  
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  
  filter(term == "BB") %>%
  
  select(HR, estimate, conf.low, conf.high)



# make ggplots

dat %>% 
  
  group_by(HR) %>%
  
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  
  filter(term == "BB") %>%
  
  select(HR, estimate, conf.low, conf.high) %>%
  
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  
  geom_errorbar() +
  
  geom_point()



# inspect with glance

glance(fit)



augment(fit)



#Question 7

library(Lahman)

library(tidyverse)

library(broom)

dat2 <- Teams %>% filter(yearID %in% 1961:2001) %>%
  
  mutate(HR = HR/G,
         
         R = R/G) %>%
  
  select(lgID, HR, BB, R)

dat2 %>%
  
  group_by(lgID) %>%
  
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>%
  
  filter(term == "HR")



#Assessment: Tibbles, do, and broom, part 2

library(tidyverse)

library(HistData)

data("GaltonFamilies")

set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later

galton <- GaltonFamilies %>%
  
  group_by(family, gender) %>%
  
  sample_n(1) %>%
  
  ungroup() %>%
  
  gather(parent, parentHeight, father:mother) %>%
  
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  
  unite(pair, c("parent", "child"))



galton



#Question 8

galton %>% group_by(pair) %>% summarise(n = n())



#Question 9

galton %>% group_by(pair) %>% summarise(Cor = cor(childHeight, parentHeight))



#Question 10a

library(broom)

fit <- galton %>% filter(pair == "father_daughter") %>%
  
  lm(childHeight ~ parentHeight, data=.)

tidy(fit)



galton %>% filter(pair == "mother_son") %>%
  
  lm(childHeight ~ parentHeight, data=.)



galton %>%
  
  group_by(pair) %>%
  
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))



#Question 10b

galton %>%
  
  group_by(pair) %>%
  
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  
  filter(term == "parentHeight") %>%
  
  mutate(condif = conf.high - conf.low) %>%
  
  mutate(nStdAbw = condif/std.error)





#Building a better offensive metric for baseball

# linear regression with two variables

fit <- Teams %>%
  
  filter(yearID %in% 1961:2001) %>%
  
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>% 
  
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)



# regression with BB, singles, doubles, triples, HR

fit <- Teams %>%
  
  filter(yearID %in% 1961:2001) %>%
  
  mutate(BB = BB / G,
         
         singles = (H - X2B - X3B - HR) / G,
         
         doubles = X2B / G,
         
         triples = X3B / G,
         
         HR = HR / G,
         
         R = R / G) %>% 
  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

coefs <- tidy(fit, conf.int = TRUE)

coefs



# predict number of runs for each team in 2002 and plot

Teams %>%
  
  filter(yearID %in% 2002) %>%
  
  mutate(BB = BB/G,
         
         singles = (H-X2B-X3B-HR)/G,
         
         doubles = X2B/G,
         
         triples =X3B/G,
         
         HR=HR/G,
         
         R=R/G)  %>%
  
  mutate(R_hat = predict(fit, newdata = .)) %>%
  
  ggplot(aes(R_hat, R, label = teamID)) +
  
  geom_point() +
  
  geom_text(nudge_x=0.1, cex = 2) +
  
  geom_abline()



# average number of team plate appearances per game

pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  
  group_by(teamID) %>%
  
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  
  pull(pa_per_game) %>%
  
  mean



# compute per-plate-appearance rates for players available in 2002 using previous data

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  
  group_by(playerID) %>%
  
  mutate(PA = BB + AB) %>%
  
  summarize(G = sum(PA)/pa_per_game,
            
            BB = sum(BB)/G,
            
            singles = sum(H-X2B-X3B-HR)/G,
            
            doubles = sum(X2B)/G,
            
            triples = sum(X3B)/G,
            
            HR = sum(HR)/G,
            
            AVG = sum(H)/sum(AB),
            
            PA = sum(PA)) %>%
  
  filter(PA >= 300) %>%
  
  select(-G) %>%
  
  mutate(R_hat = predict(fit, newdata = .))



# plot player-specific predicted runs

qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))



# add 2002 salary of each player

players <- Salaries %>%
  
  filter(yearID == 2002) %>%
  
  select(playerID, salary) %>%
  
  right_join(players, by="playerID")



# add defensive position

position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")

tmp_tab <- Appearances %>%
  
  filter(yearID == 2002) %>%
  
  group_by(playerID) %>%
  
  summarize_at(position_names, sum) %>%
  
  ungroup() 

pos <- tmp_tab %>%
  
  select(position_names) %>%
  
  apply(., 1, which.max)

players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  
  filter(POS != "P") %>%
  
  right_join(players, by="playerID") %>%
  
  filter(!is.na(POS)  & !is.na(salary))



# add players' first and last names

players <- Master %>%
  
  select(playerID, nameFirst, nameLast, debut) %>%
  
  mutate(debut = as.Date(debut)) %>%
  
  right_join(players, by="playerID")



# top 10 players

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  
  arrange(desc(R_hat)) %>%
  
  top_n(10)



# players with a higher metric have higher salaries

players %>% ggplot(aes(salary, R_hat, color = POS)) +
  
  geom_point() +
  
  scale_x_log10()



# remake plot without players that debuted after 1998

library(lubridate)

players %>% filter(year(debut) < 1998) %>%
  
  ggplot(aes(salary, R_hat, color = POS)) +
  
  geom_point() +
  
  scale_x_log10()





#Building a Better Offensive Metric for Baseball: Linear Programming

library(reshape2)

library(lpSolve)

library(Lahman)

library(tidyverse)



players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")

constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)

npos <- nrow(constraint_matrix)

constraint_matrix <- rbind(constraint_matrix, salary = players$salary)

constraint_dir <- c(rep("==", npos), "<=")

constraint_limit <- c(rep(1, npos), 50*10^6)

lp_solution <- lp("max", players$R_hat,
                  
                  constraint_matrix, constraint_dir, constraint_limit,
                  
                  all.int = TRUE)



our_team <- players %>%
  
  filter(lp_solution$solution == 1) %>%
  
  arrange(desc(R_hat))

our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)



my_scale <- function(x) (x - median(x))/mad(x)

players %>% mutate(BB = my_scale(BB),
                   
                   singles = my_scale(singles),
                   
                   doubles = my_scale(doubles),
                   
                   triples = my_scale(triples),
                   
                   HR = my_scale(HR),
                   
                   AVG = my_scale(AVG),
                   
                   R_hat = my_scale(R_hat)) %>%
  
  filter(playerID %in% our_team$playerID) %>%
  
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  
  arrange(desc(R_hat))





#Regression Fallacy

#The code to create a table with player ID, their names, and their most played position:

library(Lahman)

playerInfo <- Fielding %>%
  
  group_by(playerID) %>%
  
  arrange(desc(G)) %>%
  
  slice(1) %>%
  
  ungroup %>%
  
  left_join(Master, by="playerID") %>%
  
  select(playerID, nameFirst, nameLast, POS)

#The code to create a table with only the ROY award winners and add their batting statistics:

ROY <- AwardsPlayers %>%
  
  filter(awardID == "Rookie of the Year") %>%
  
  left_join(playerInfo, by="playerID") %>%
  
  rename(rookie_year = yearID) %>%
  
  right_join(Batting, by="playerID") %>%
  
  mutate(AVG = H/AB) %>%
  
  filter(POS != "P")

#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:

ROY <- ROY %>%
  
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  
  group_by(playerID) %>%
  
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  
  filter(n() == 2) %>%
  
  ungroup %>%
  
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#The code to use the spread function to have one column for the rookie and sophomore years batting averages:

ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

ROY

#The code to calculate the proportion of players who have a lower batting average their sophomore year:

mean(ROY$sophomore - ROY$rookie <= 0)

#The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):

two_years <- Batting %>%
  
  filter(yearID %in% 2013:2014) %>%
  
  group_by(playerID, yearID) %>%
  
  filter(sum(AB) >= 130) %>%
  
  summarize(AVG = sum(H)/sum(AB)) %>%
  
  ungroup %>%
  
  spread(yearID, AVG) %>%
  
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  
  left_join(playerInfo, by="playerID") %>%
  
  filter(POS!="P") %>%
  
  select(-POS) %>%
  
  arrange(desc(`2013`)) %>%
  
  select(nameFirst, nameLast, `2013`, `2014`)

two_years



#The code to see what happens to the worst performers of 2013:

arrange(two_years, `2013`)

#The code to see  the correlation for performance in two separate years:

qplot(`2013`, `2014`, data = two_years)



summarize(two_years, cor(`2013`,`2014`))



#Measurement Error Models

#The code to use dslabs function rfalling_object to generate simulations of dropping balls:

library(dslabs)

library(broom)

falling_object <- rfalling_object()

#The code to draw the trajectory of the ball:

falling_object %>%
  
  ggplot(aes(time, observed_distance)) +
  
  geom_point() +
  
  ylab("Distance in meters") +
  
  xlab("Time in seconds")

#The code to use the lm() function to estimate the coefficients:

fit <- falling_object %>%
  
  mutate(time_sq = time^2) %>%
  
  lm(observed_distance~time+time_sq, data=.)



tidy(fit)

#The code to check if the estimated parabola fits the data:

augment(fit) %>%
  
  ggplot() +
  
  geom_point(aes(time, observed_distance)) +
  
  geom_line(aes(time, .fitted), col = "blue")

#The code to see the summary statistic of the regression:

tidy(fit, conf.int = TRUE)





#Question 3

# code zeile 719 laufen lassen: predict number of runs for each team in 2002 and plot

# predict number of runs for each team in 2002 and plot

Q3teams <- data.frame(team = c("A", "B"), BB= c(2, 1), singles = c(4, 6), doubles = c(1, 2), triples = c(0, 1), HR = c(1, 0))

predict(fit, newdata = Q3teams)



#Question 9

library(Lahman)

data(Teams)

fitQ9 <- Teams %>% filter(yearID == 1971) %>%
  
  mutate(BB = BB / G,
         
         HR = HR/G,
         
         R = R/G) %>%
  
  lm(R ~ BB + HR, data = .)

tidy(fitQ9)



#Question 10

library(broom)

datQ10 <- Teams %>% filter(yearID %in% 1961:2018) %>%
  
  mutate(BB = BB / G,
         
         HR = HR/G,
         
         R = R/G) %>%
  
  group_by(yearID) %>%
  
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  
  ungroup() %>%
  
  filter(term == "BB")



datQ10 %>% ggplot(aes(yearID, estimate)) +
  
  geom_point() +
  
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) # nicht sinnvoll, besser geom_smooth



res <- Teams %>%
  
  filter(yearID %in% 1961:2018) %>%
  
  group_by(yearID) %>%
  
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  
  ungroup()

res %>%
  
  filter(term == "BB") %>%
  
  ggplot(aes(yearID, estimate)) +
  
  geom_point() +
  
  geom_smooth(method = "lm")



#Question 11

tidy(lm(estimate ~ yearID, data = datQ10))

tidy(lm(estimate ~ yearID, data = res %>% filter(term == "BB")))
