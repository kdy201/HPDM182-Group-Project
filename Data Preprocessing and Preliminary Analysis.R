library(tidyverse)
library(ggplot2)
library(plotly)

breast_cancer <- breast_cancer_number_of_new_female_cases

#Check the number of years available
nrow(breast_cancer)
head(breast_cancer)
2021 - 1990


bmi <- body_mass_index_bmi_women_kgperm2_1_
bmi
ncol(bmi)
nrow(bmi)

2008 - 1999


#2004 to 2008


#Data Preprocessing
#1. Find overlapping countries, filter it
#2. Take from 2004 - 2008

#Fitting as a normal linear model
#H0: There is no relationship between breast cancer incidence and bmi among different countries
#H1: There is 

bmi_countries_involved <- view(bmi['name'])
breast_cancer_involved <- view(breast_cancer['name'])


#Filter out countries 
countries_overlap <- intersect(bmi_countries_involved, breast_cancer_involved)

print(countries_overlap, n=190)

library(dplyr)
filtered_breast_cancer <- breast_cancer %>%
  filter(name %in% countries_overlap$name)


filtered_breast_cancer


fil

filtered_bmi <- bmi%>% 
  filter(name %in% countries_overlap)

unique(breast_cancer$name)
unique(countries_overlap)




filtered_bmi
filtered_breast_cancer




filtered_bmi %>% select('name',`2004`:`2008`)

filtered_breast_cancer %>% select('name',`2004`:`2008`)



merged_data <- filtered_bmi %>%
  select(name, `2004`:`2008`) %>%
  inner_join(
    filtered_breast_cancer %>% select(name, `2004`:`2008`),
    by = "name",
    suffix = c("_bmi", "_breast_cancer")
  )


merged_data


write.csv(merged_data ,"~/HPDM182- Group-Project/merged_data.csv", row.names = FALSE)



merged_data


lm <- lm( '2004_breast_cancer'~ '2004_bmi', data = merged_data) #Create the linear regression


lm_model <- lm(`2004_breast_cancer` ~ `2004_bmi`, data = merged_data)
summary(lm_model)


plot(merged_data$`2004_bmi`, merged_data$`2004_breast_cancer`,
     xlab = "BMI (2004)", ylab = "Breast Cancer (2004)",
     main = "BMI vs Breast Cancer (2004)")
abline(lm_model, col = "blue", lwd = 2)


library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# Select only the needed columns
data_long <- merged_data %>%
  select(name, starts_with("2004"), starts_with("2005"), starts_with("2006"), starts_with("2007"), starts_with("2008")) %>%
  pivot_longer(cols = -name,
               names_to = c("year", "variable"),
               names_pattern = "(\\d{4})_(.*)",
               values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value)

# Run a regression per year
results <- data_long %>%
  group_by(year) %>%
  do(tidy(lm(breast_cancer ~ bmi, data = .))) %>%
  ungroup()

# Print slope and p-value per year
reg_summary <- results %>%
  filter(term == "bmi") %>%
  select(year, estimate, p.value)

print(reg_summary)

# Plot BMI vs Breast Cancer for each year
ggplot(data_long, aes(x = bmi, y = breast_cancer)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
  facet_wrap(~ year, scales = "free") +
  labs(title = "BMI vs Breast Cancer (2004–2008)",
       x = "BMI",
       y = "Breast Cancer Cases") +
  theme_minimal(base_size = 14)

merged_data
merged_data['2004_bmi','name']
# Select only the 'name' and '2004_bmi' columns
average_bmi_2004 <- merged_data[, c("name", "2004_bmi")]

#BMI AND CASES

hist(merged_data$'2004_bmi')
hist(merged_data$'2004_breast_cancer')

library(dplyr)

# Filter rows where 2004_bmi > 25
merged_data_above_25 <- merged_data %>%
  filter(`2004_bmi` > 25.0)



# View the filtered data
hist(merged_data_above_25$'2004_bmi')

lm_above_25 <- lm(`2004_breast_cancer` ~ log(`2004_bmi`), data = merged_data_above_25)
summary(lm_above_25)

merged_data$'2004_breast_cancer'



