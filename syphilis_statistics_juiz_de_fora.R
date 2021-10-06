# requiring librarys ------------------------------------------------------
library(tidyverse)
theme_set(theme_classic())

# Acquired Syphilis -------------------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

year <- 2010:2019
rate <- c(0,
          0,
          1.31,
          3.15,
          10.27,
          18.72,
          34.59,
          85.06,
          163.21,
          180.18)

bd <- data.frame(year,rate)
bdf <- data.frame(year = bd$year[-c(1,2)], rate = bd$rate[-c(1,2)])

# fitting data in a log linear model 

yearspos <- 1:8 #2010 and 2011 values are 0, thus they were removed
lmexp <- lm(log(bdf$rate)~yearspos) #starting in 2012
a <- exp(as.numeric(lmexp$coefficients[2])) #value of 'a' coefficient
b <- exp(as.numeric(lmexp$coefficients[1])) #valeu of 'b' coefficient

r2 <- summary(lmexp)$r.squared # value of R2 

## plotting the exponential regression

RE_1 <- ggplot(bdf, aes(x = 1:8, y = rate)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((b*a^x)),
               color = "black") + #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8),
    labels = c("2012","2013","2014","2015","2016","2017","2018","2019")) +
  ggtitle("Detection Rate of Acquired Syphilis \n in Juiz de Fora between 2012 and 2019") +
  # theme_bw() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) 
#saving plot
ggsave("acquired_syphilis.tiff", dpi = 1200 )

# Yearly estimate growth rate of acquired syphilis 

p2.5 <- exp(confint(lmexp)[2,1]) - 1  # 2.5% percentile of CI 95%
p50 <- a -  1                         # estimate
p97.5 <- exp(confint(lmexp)[2,2]) - 1 # 97.5% percentile of CI 95%

# Syphilis in pregnant women ----------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

year <- 2010:2019
yearpos <- 1:10

taxa <- c(1.13,
          1.38,
          2.21,
          5.23,
          7.83,
          11.75,
          10.39,
          18.83,
          36.68,
          41.96)

bd <- data.frame(year,taxa)
bdf <- data.frame(yearpos,taxa)

# fitting data in a log linear model 

lmexp <- lm(log(taxa)~yearpos)
a <- exp(as.numeric(lmexp$coefficients[2])) #value of 'a' coefficient
b <- exp(as.numeric(lmexp$coefficients[1])) #valeu of 'b' coefficient

r2 <- summary(lmexp)$r.squared # value of R2 


## plotting the exponential regression

RE_2 <- ggplot(bdf, aes(x = yearpos, y = taxa)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((b*a^x)),
               color = "black") +  #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("2010","2011","2012","2013","2014",
               "2015","2016","2017","2018","2019")) +
  ggtitle("Detection Rate of Syphilis in Pregnant Women in \n Juiz de Fora between 2010 and 2019") +
  # theme_bw() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) 

# Saving plot
ggsave("pregnant_syphilis.tiff",dpi = 1200)

# Yearly estimate growth rate of syphilis in pregnant women

p2.5 <- exp(confint(lmexp)[2,1]) - 1  # 2.5% percentile of CI 95%
p50 <- a -  1                         # estimate 
p97.5 <- exp(confint(lmexp)[2,2]) - 1 # 97.5% percentile of CI 95%

# Congenital Syphilis  ----------------------------------------------------

# Building up syphilis's dataset based on http://indicadoressifilis.aids.gov.br

year <- 2010:2019
yearpos <- 1:10

taxa <- c(0.48,
          1.68,
          4.56,
          5.39,
          9.16,
          10.26,
          10.70,
          10.48,
          16.11,
          14.25)

bd <- data.frame(year,taxa)
bdf <- data.frame(yearpos,taxa)

# fitting data in a log linear model 

lmexp <- lm(log(taxa)~yearpos)
a <- exp(as.numeric(lmexp$coefficients[2])) #value of 'a' coefficient
b <- exp(as.numeric(lmexp$coefficients[1])) #valeu of 'b' coefficient

r2 <- summary(lmexp)$r.squared # value of R2 


## plotting the exponential regression

RE_3 <- ggplot(bdf, aes(x = yearpos, y = taxa)) +
  geom_point() +
  geom_smooth (method = "lm", 
               formula = y ~ I((b*a^x)),
               color = "black") + #fitting curve in a regular exponential function
  ylab("Detection Rate") +
  xlab("Year") +
  scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("2010","2011","2012","2013","2014",
               "2015","2016","2017","2018","2019")) +
  ggtitle("Detection Rate of Congenital Syphilis in \n Juiz de Fora between 2010 and 2019") +
  # theme_bw() +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(t = 0, r = 0, b = 20, l = 0))) 
# Saving plot
ggsave("congenital_syphilis.tiff", dpi = 1200)

# Yearly estimate growth rate of syphilis in pregnant women

p2.5 <- exp(confint(lmexp)[2,1]) - 1  # 2.5% percentile of CI 95%
p50 <- a -  1                         # estimate
p97.5 <- exp(confint(lmexp)[2,2]) - 1 # 97.5% percentile of CI 95%

#Uniting plots

plot_top = plot_grid(RE_1,
                     RE_2, 
                     labels = 'AUTO',
                     vjust = -2 ,
                     label_x = 0, 
                     label_y = 0)
plot_bottom = plot_grid(NULL,
                        RE_3, 
                        NULL, 
                        ncol=3, 
                        rel_widths=c(0.25,0.5,0.25), 
                        labels = '',
                        vjust = -3,
                        hjust = -17,
                        label_x = 0, 
                        label_y = 0)


unite_RE_plot <- cowplot::plot_grid(plot_top, plot_bottom,
                                     ncol = 1, 
                                     labels = c("","C"),
                                     vjust = -2 ,
                                     label_x = 0.2, 
                                     label_y = 0)

ggsave('unite_RE_plot.tiff',
       plot = unite_RE_plot, 
       dpi= 1200, 
       width = 10000,
       height = 7000, 
       units = "px")
