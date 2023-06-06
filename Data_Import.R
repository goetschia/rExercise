rm(list = ls() ) 
library(tidyverse)
library(lubridate)
library(gtsummary)
library(labelled)
dat <- read_csv("insurance_with_date.csv")

dat <- dat %>% mutate(across(c(sex, region), factor),
                      child.3 = as.numeric(children > 2),
                      smoke_bin = as.numeric(str_detect(smoker, "yes")),
                      date = date + months(6) )

ggplot(dat, aes(x = bmi, fill = sex, colour = sex )) + geom_density(alpha = .5) + 
  xlab(label = "BMI (kg/m^2)") + theme(legend.position = "bottom")

ggplot(dat, aes(x = charges)) + geom_histogram(binwidth = 1000, alpha = 0.5, aes(y = after_stat(density),colour = sex, fill = sex)) +
  geom_density(aes(colour = sex), alpha = 1, linewidth = 1.5) + geom_vline(xintercept = median(dat$charges, na.rm = T), colour = "red", linewidth = 1.5)+
  xlab(label = "Charges in dollars")


ggplot(dat, aes(x= age, y = bmi, colour = smoker)) + geom_quantile() + geom_point() +
  xlab(label = "Age (years)") + ylab(label = expression(paste("BMI (kg/", m^2 ,")"))) +theme(legend.position = "bottom")


ggplot(dat, aes(x = smoker, y= charges)) + geom_violin() + ylab( label = "charges ($)")

ggplot(dat, aes(x = smoker, y = charges)) + geom_boxplot() + coord_flip() +ylab( label = "charges ($)")
