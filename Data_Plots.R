rm(list = ls() ) 
library(tidyverse)
library(unibeCols)
library(cowplot)


dat <- read_csv("ebola.csv")
dat <- dat %>% arrange(Date)
dat_plot <- dat %>% select(c(Country, Date, Cum_conf_cases))
dat_plot <- dat_plot %>% filter((Date <= as.Date("2015-03-31")) & (Country == "Liberia" | Country == "Sierra Leone" | Country == "Nigeria"))

dat_plot <- dat_plot %>% group_by(Country, Date) %>% mutate(Cases = n())

ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases, colour = Country, fill = Country)) + geom_point( alpha = .7,  shape = 20, size=1.5, stroke = 1.5) +
    xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_x_date(breaks =  as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-01-01", "2015-04-01")),
               labels = c("29 August", "01 October", "01 December", "01 January", "01 April")) + theme_bw() + facet_grid(cols = vars(Country))

  
ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases)) + geom_line(mapping = aes(colour = Country), alpha = 0.7, linetype = "solid", linewidth = 1.5 ) +
  xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_y_continuous(breaks = c(0,2500,5000, 7500,10000)) + theme_bw() + facet_grid(cols = vars(Country))

ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + geom_col(position="stack", linewidth= 0.5, width = 1) +
  xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_y_continuous(breaks = c(0,2500,5000, 7500,10000)) + theme_bw()+ facet_grid(cols = vars(Country))

# Patchworking
p_point <- ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases, colour = Country, fill = Country)) + geom_point( alpha = .7,  shape = 20, size=1.5, stroke = 1.5) +
  xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_x_date(breaks =  as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-01-01", "2015-04-01")),
               labels = c("29 August", "01 October", "01 December", "01 January", "01 April")) + theme_bw() 


p_line <- ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases)) + geom_line(mapping = aes(colour = Country), alpha = 0.7, linetype = "solid", linewidth = 1.5 ) +
  xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_y_continuous(breaks = c(0,2500,5000, 7500,10000)) + theme_bw() 

p_col <- ggplot(dat_plot, aes(x = Date, y = Cum_conf_cases, fill = Country, colour = Country)) + geom_col(position="stack", linewidth= 0.5, width = 1) +
  xlab(label = "Time") + ylab(label = "# of confirmed cases") + ggtitle(label = "Confirmed ebola cases in three countries") +
  scale_color_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_fill_manual(name = "Country", breaks = c("Liberia", "Nigeria", "Sierra Leone"), values = c(unibePastelS()[1], unibeIceS()[1], unibeGreenS()[1])) +
  scale_y_continuous(breaks = c(0,2500,5000, 7500,10000)) + theme_bw()

p_grid <- plot_grid(plotlist = list(p_point, p_line, p_col), labels = c("V1", "V2", "V3"),
                    label_size = 12, nrow = 2)









