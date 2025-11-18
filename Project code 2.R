library(tidyverse)


library(plotly)


#GDP
gdp <- gdp_pcap

gdp

#Breast Cancer Cases



#Lower income, middle income, higher income -> Interactions
#Two linear models, predicting mortality and number of cases

# Select geo, name, and columns from 1998 to 2008
df_1998_2008 <- gdp %>%
  select(geo, name, `1998`:`2008`)
View(df_1998_2008)

df_1998_2008

library(dplyr)

df_classified <- df_1998_2008 %>%
  # 1. Calculate average income over 1998-2008
  rowwise() %>%
  mutate(avg_income = mean(c_across(`1998`:`2008`), na.rm = TRUE)) %>%
  ungroup() %>%
  # 2. Classify into Low, Mid, High
  mutate(
    income_class = case_when(
      avg_income <= 1145 ~ "Low",
      avg_income >= 1146 & avg_income < 4515 ~ "Mid",
      avg_income >= 4516 & avg_income <14005 ~ "Upper-middle",
      avg_income >= 14005 ~ "High"
    )
  )



df_gdp <- df_classified %>%
  select(geo, name, avg_income, income_class)
df_gdp



#Deaths 

breast_cancer_deaths_per_100000_women
df_deaths <- breast_cancer_deaths_per_100000_women
df_deaths
df_deaths <-  df_deaths %>%
  select(geo, name, `1998`:`2008`)
df_deaths
df_deaths <- df_deaths %>%
  rowwise() %>%  # allows row-wise calculations
  mutate(avg_deaths = mean(c_across(`1998`:`2007`), na.rm = TRUE)) %>%
  ungroup()
df_deaths <- df_deaths%>% 
  select(geo,name,avg_deaths)
df_deaths


#Number of cases
breast_cancer_new_cases_per_100000_women
df_cases <- breast_cancer_new_cases_per_100000_women
df_cases <-  df_cases %>%
  select(geo, name, `1998`:`2008`)
df_cases <- df_cases %>%
  rowwise() %>%  # allows row-wise calculations
  mutate(avg_cases = mean(c_across(`1998`:`2007`), na.rm = TRUE)) %>%
  ungroup()

df_cases
df_cases <- df_cases%>% 
  select(geo,name,avg_cases)

df_cases


#Merge by Left Join:
library(dplyr)

# Merge df_cases and df_death first
df_merged <- df_cases %>%
  left_join(df_deaths, by = c("geo", "name")) %>%
  left_join(df_gdp, by = c("geo", "name"))



View(df_merged)


#Linear Model: Cases
model=lm_cases <- lm(avg_cases ~ avg_income +income_class, data=df_merged)
model %>% summary()
model2=lm(avg_deaths ~ avg_income + income_class, data=df_merged)
model2 %>% summary()

#Linear Model: Mortality 
lm_mortality <- lm(avg_deaths ~ avg_income* income_class_num, data=df_merged)
lm_mortality %>% summary()



#Pearson or Spearman 
df_merged

######## Which one correlates more?

df_grouped <- df_merged %>%
  group_by(income_class_num) %>%
  summarise(
    avg_deaths_mean = mean(avg_deaths),
    avg_cases_mean = mean(avg_cases),
    avg_income_mean = mean(avg_income),
    .groups = "drop"
  )

df_grouped
# Calculate correlation between avg_cases and avg_deaths for each income group
df_correlations <- df_merged %>%
  group_by(income_class_num) %>%
  summarise(
    correlation = cor(avg_cases, avg_deaths, use = "complete.obs"),
    n_countries = n(),              # optional: to see how many countries per group
    .groups = "drop"
  )

df_correlations <- df_merged %>%
  group_by(income_class_num) %>%
  summarise(
    correlation = cor(avg_income, avg_cases, use = "complete.obs"),
    .groups = "drop"
  )
df_correlations

df_correlations_deaths <- df_merged %>%
  group_by(income_class_num) %>%
  summarise(
    correlation = cor(avg_income, avg_deaths, use = "complete.obs"),
    .groups = "drop"
  )
df_correlations_deaths


ggplot(df_merged, aes(x = avg_income, y = avg_cases)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ income_class_num, scales = "free") +
  labs(title = "GDP vs Cases Across Income Groups",
       x = "Average GDP per Capita",
       y = "Average Cases")

#World Map 

anova(model)

