install.packages("tidyverse")
install.packages("magrittr")
install.packages("tidymodels")

library(tidyverse)
library(dplyr)
library(tibble)
library(tidymodels)
library(randomForest)
library(conflicted)
conflicts_prefer(tidyr::extract)

setwd("C:/Users/danie/Documents/2. semester/oznal/R files/zadanie/")
df <- read_csv("PLACES__ZCTA_Data_(GIS_Friendly_Format),_2025_release_20260423.csv")
View(df)

# ══════════════════════════════════════════════════════════════════════════════
# EDA
# ══════════════════════════════════════════════════════════════════════════════

# What size, columns, ...
dim(df)
colnames(df)

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
df <- df %>%
  rename_with(~ str_replace_all(., "_CrudePrev", ""))

# Summary statistics - min, max, mean, median

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
summary(df)

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
# Linear Regression
# Decision Tree
# Random Forest

# 2
# Parametric: Linear Regression, Linear Discriminant Analysis (LDA), Probit Regression
# Nonparametric: k-Nearest Neighbors (k-NN), Decision Tree, Support Vector Machine (SVM)

# 3
# one algorithmic: stepwise selection
# two embedded: LASSO, ridge regression

# ══════════════════════════════════════════════════════════════════════════════
# HYPOTHESIS & PROJECT SETUP
# ══════════════════════════════════════════════════════════════════════════════

# Target variable: GHLTH_CrudePrev
# (Crude prevalence of fair or poor self-rated general health, %)
#
# Features (behavioral & socioeconomic determinants):
#   - CSMOKING_CrudePrev   : Current smoking
#   - BINGE_CrudePrev      : Binge drinking
#   - LPA_CrudePrev        : Physical inactivity
#   - SLEEP_CrudePrev      : Sleep deprivation
#   - OBESITY_CrudePrev    : Obesity
#   - ACCESS2_CrudePrev    : Lack of health insurance / access to care
#   - FOODINSECU_CrudePrev : Food insecurity
#   - HOUSINSECU_CrudePrev : Housing insecurity
#   - SHUTUTILITY_CrudePrev: Utility shutoff risk
#   - TotalPop18plus       : Population size (control variable)
#
# Main hypothesis:
#   General health outcomes at the ZCTA level are significantly predicted
#   by behavioral and socioeconomic factors. We expect that smoking,
#   physical inactivity, and food/housing insecurity will be the strongest
#   predictors of poor self-rated health.
#
# Scenario 1 hypothesis:
#   Tree-based methods (recursive partitioning) will outperform linear
#   partitioning in predictive accuracy, as relationships between
#   health determinants and GHLTH are likely nonlinear.
#   We expect key features (smoking, obesity, inactivity) to remain
#   informative across both partitioning families.
#
# Scenario 2 hypothesis:
#   Nonparametric models will achieve higher accuracy than parametric
#   models due to the nonlinear nature of health outcome data.
#   However, parametric models will offer stronger explainability
#   and reproducibility.
#
# Scenario 1 — 3 methods, 2 feature-space partitioning approaches:
#   Linear Partitioning:    Linear Regression
#   Recursive Partitioning: Decision Tree (CART), Random Forest
#
# Scenario 2 — Parametric vs Nonparametric (3 + 3 methods):
#   Parametric:    Linear Regression, Ridge Regression, Lasso Regression
#   Nonparametric: Decision Tree (CART), Random Forest, k-NN Regression

# ══════════════════════════════════════════════════════════════════════════════
# PREPROCESSING
# ══════════════════════════════════════════════════════════════════════════════

# Fix TotalPop18Plus - stored as string with comma formatting (e.g. "14,019")
df <- df %>%
  mutate(TotalPop18plus = as.numeric(gsub(",", "", TotalPop18plus)))

# Select relevant columns
selected_cols <- c(
  "GHLTH_CrudePrev",
  "TotalPop18plus",
  "ACCESS2_CrudePrev",
  "BINGE_CrudePrev",
  "CSMOKING_CrudePrev",
  "LPA_CrudePrev",
  "SLEEP_CrudePrev",
  "OBESITY_CrudePrev",
  "FOODINSECU_CrudePrev",
  "HOUSINSECU_CrudePrev",
  "SHUTUTILITY_CrudePrev"
)

df_model <- df %>%
  select(all_of(selected_cols)) %>%
  na.omit()



df_clean <- df %>%
  select(ends_with("_CrudePrev"), TotalPop18plus, TotalPopulation) %>%
  drop_na()

data_split <- initial_split(df, prop = 0.8)

train_data <- training(data_split)
test_data  <- testing(data_split)

df <- df %>%
  mutate(STROKE_CrudePrev = as.numeric(STROKE_CrudePrev)) %>%
  filter(!is.na(STROKE_CrudePrev))

# Random forest
rand_forest <- randomForest(formula = VISION_CrudePrev ~ SLEEP_CrudePrev+STROKE_CrudePrev,
                            data=df_clean, 
                            ntree=500, 
                            mtry=5, 
                            nodesize=5, 
                            maxnodes=30, 
                            replace=TRUE, 
                            importance=TRUE)
print(rand_forest)
importance(rand_forest)
View(rand_forest)

# Linear regression
lm.simple <- df_clean  %>% 
  lm(GHLTH_CrudePrev ~ CSMOKING_CrudePrev + BINGE_CrudePrev + OBESITY_CrudePrev + SLEEP_CrudePrev, .) 

lm.simple
summary(lm.simple)
tidy(lm.simple)
View(lm.simple)
