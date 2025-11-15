#Download packages
library(tidyverse)
library(plotly)
library(dplyr)

#import csv files
gdp <- read.csv('gdp_pcap.csv')
BC <- read.csv('breast_cancer_new_cases_per_100000_women.csv')
BMI <- read.csv('body_mass_index_bmi_women_kgperm2 (1).csv')


gdp4_8 = gdp %>% select('name','X2004':'X2008')
BC4_8 = BC %>% select('name','X2004':'X2008')
BMI4_8 = BMI %>% select('name','X2004':'X2008')

#listing the oecd countries
oecd <- c("Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica",
          "Czech Republic","Denmark","Estonia","Finland","France","Germany",
          "Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan",
          "South Korea","Latvia","Lithuania","Luxembourg","Mexico","Netherlands",
          "New Zealand","Norway","Poland","Portugal","Slovak Republic",
          "Slovenia","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States")

#filtering the data to 2004 - 2008, oecd countries only
oecd_BC= BC4_8 %>% filter(name %in% oecd)
oecd_BMI= BMI4_8 %>% filter(name %in% oecd)
oecd_GDP= gdp4_8 %>% filter(name %in% oecd)

#finding the average value across the 5 years
oecd_BC$ave_BC= rowMeans(oecd_BC[, c('X2004','X2005','X2006','X2007','X2008')], na.rm = TRUE)
oecd_BMI$ave_BMI= rowMeans(oecd_BMI[, c('X2004','X2005','X2006','X2007','X2008')], na.rm = TRUE)
oecd_GDP$ave_GDP= rowMeans(oecd_GDP[, c('X2004','X2005','X2006','X2007','X2008')], na.rm = TRUE)

#merging all variables of interest
merged1= merge(oecd_BC[, c('name','ave_BC')],
              oecd_BMI[, c('name','ave_BMI')],
              by='name')

merged=merge(merged1,
             oecd_GDP[, c('name','ave_GDP')],
             by='name')
#changing name column to oecd_countries
merged =merged %>% rename( oecd_countries = name)

#normality of breast cancer cases using histogram
plot_ly(x = merged$ave_BC, type = "histogram", xbins = list(size = 15)) %>%
  layout(bargap = 0.1,yaxis = list(title = ' breast cancer cases'),xaxis = list(title = 'no of breast cancer cases'))


lm= lm(ave_BC~ ave_BMI + ave_GDP, data=merged) %>% summary()


ggplot(merged,aes(x=ave_BMI,y=ave_BC))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_bw()+
  theme(text = element_text(size = 20))







