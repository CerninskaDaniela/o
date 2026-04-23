install.packages("tidyverse")
install.packages("magrittr")

library(tidyverse)

setwd("C:/Users/danie/Documents/2. semester/oznal/R files/zadanie/")
df <- read_csv("PLACES__ZCTA_Data_(GIS_Friendly_Format),_2025_release_20260423.csv")
View(df)

# What size, columns, ...
dim(df)

# Check duplicates
duplicates <- df %>% 
  filter(duplicated(ZCTA5))
duplicates

# Check missing values

df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column_name", values_to = "na_count") %>%
  print(n = Inf)

df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column_name", values_to = "na_count") %>%
  mutate(
    percentage = (na_count / nrow(df)) * 100
  ) %>%
  print(n = Inf)

df %>%
  mutate(na_per_row = rowSums(is.na(.))) %>%
  count(na_per_row) %>%
  mutate(percentage = (n / sum(n)) * 100)


# Convenient value formats, column names


# Summary statistics - min, max, mean, median, mode

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

final_summary <- df %>%
  summarise(across(where(is.numeric), list(
    Min    = ~ min(.x, na.rm = TRUE),
    Mean   = ~ mean(.x, na.rm = TRUE),
    Median = ~ median(.x, na.rm = TRUE),
    Mode   = ~ get_mode(.x),
    Max    = ~ max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "temp_name", values_to = "value") %>%
  extract(temp_name, into = c("Variable", "Stat"), regex = "(.*)_(.*)") %>%
  pivot_wider(names_from = Stat, values_from = value)

print(final_summary, n = Inf)

# Value distribution


# Vizualizations

df %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~variable, scales = "free") + 
  theme_minimal() +
  labs(title = "Histograms of Numeric Variables", x = "Value", y = "Frequency")

df %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(y = value, x = "")) + # x is empty to create a single vertical box per facet
  geom_boxplot(fill = "orange", outlier.color = "red", outlier.shape = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Numeric Variables", y = "Value", x = NULL)


# What to do with column with numerous values?


# Column names are okay?


# Methods and models

# examples to tasks
# 1 
# Logistic Regression (Statistical Family / Linear Partitioning)
# Decision Tree (Machine Learning Family / Recursive Partitioning)
# Random Forest (Machine Learning Family / Recursive Partitioning)

# 2
# Parametric: Logistic Regression, Linear Discriminant Analysis (LDA), Probit Regression
# Nonparametric: k-Nearest Neighbors (k-NN), Decision Tree (CART), Support Vector Machine (SVM) with RBF Kernel

# 3
# one algorithmic: stepwise selection
# two embedded: LASSO, ridge regression
