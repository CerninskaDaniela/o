install.packages("tidyverse")
install.packages("magrittr")
install.packages("tidymodels")
install.packages("e1071")
install.packages("corrplot")
install.packages(c("rpart", "rpart.plot", "glmnet", "caret"))


library(tidyverse)
library(dplyr)
library(tibble)
library(tidymodels)
library(randomForest)
library(e1071)
library(corrplot)
library(rpart); library(rpart.plot); library(glmnet); library(caret)


setwd("C:/Users/danie/Documents/2. semester/oznal/R files/zadanie/")
df <- read_csv("PLACES__ZCTA_Data_(GIS_Friendly_Format),_2025_release_20260423.csv")
View(df)

# ══════════════════════════════════════════════════════════════════════════════
# EDA
# ══════════════════════════════════════════════════════════════════════════════

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

# Skewness of features

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

# Check how many rows remain after removing missing values
cat("Rows before:", nrow(df), "\n")
cat("Rows after:", nrow(df_model), "\n")

# Log transform population variable
df_model <- df_model %>%
  mutate(TotalPop18plus = log1p(TotalPop18plus))

# Verify final modeling dataset
dim(df_model)
summary(df_model)
str(df_model)

# Train/Test split
set.seed(42)

data_split <- initial_split(df_model, prop = 0.8)
train <- training(data_split)
test <- testing(data_split)

cat("Train:", nrow(train), "\n")
cat("Test:", nrow(test), "\n")

# ── LINEAR REGRESSION: Diagnostika a normalita residuálov ────────────────────
lm_model <- lm(GHLTH_CrudePrev ~ ., data = train)
summary(lm_model)

# Visual check
par(mfrow = c(2, 2))
plot(lm_model)
par(mfrow = c(1, 1))

# Residual histogram
residuals_df <- tibble(residuals = residuals(lm_model))

ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  theme_minimal() +
  labs(title = "Distribution of Residuals - Linear Regression",
       x = "Residual", y = "Count")

ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "Q-Q Plot of Residuals - Linear Regression",
       x = "Theoretical Quantiles", y = "Sample Quantiles")

set.seed(42)
shapiro_sample <- sample(residuals(lm_model), 5000)
shapiro.test(shapiro_sample)

# Skewness of features
df_model %>%
  summarise(across(everything(), ~round(skewness(.x), 3))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Skewness") %>%
  arrange(desc(abs(Skewness))) %>%
  print(n = Inf)
# Note: ACCESS2, FOODINSECU, HOUSINSECU, SHUTUTILITY show high right skew (>1.5).
# This reflects real-world inequality distributions and is expected for
# socioeconomic prevalence data. No transformation applied to these features
# as tree-based models are robust to skewness, and linear regression
# assumptions apply to residuals, not input features.

# Corelation
cor_with_target <- df_model %>%
  cor() %>%
  as.data.frame() %>%
  select(GHLTH_CrudePrev) %>%
  rownames_to_column("Feature") %>%
  filter(Feature != "GHLTH_CrudePrev") %>%
  arrange(desc(abs(GHLTH_CrudePrev)))

print(cor_with_target)

ggplot(cor_with_target, aes(x = reorder(Feature, GHLTH_CrudePrev), 
                            y = GHLTH_CrudePrev, fill = GHLTH_CrudePrev > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("Negative", "Positive"), name = "Direction") +
  theme_minimal() +
  labs(title = "Feature Correlation with Target (GHLTH_CrudePrev)",
       x = NULL, y = "Pearson Correlation")

# ── EDA: Multikolinearita medzi features ─────────────────────────────────────
install.packages("corrplot")
library(corrplot)

cor_matrix <- df_model %>%
  select(-GHLTH_CrudePrev) %>%
  cor()

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.7, addCoef.col = "black", number.cex = 0.6,
         title = "Feature Correlation Matrix", mar = c(0,0,1,0))


# ── LINEAR REGRESSION: Interpretácia normality ────────────────────
# Q-Q Plot:
# Residuals closely follow the reference line in the central part of the
# distribution. Deviations are visible only in the tails (heavy tails), which
# is typical for large real-world datasets. The central region, representing the
# majority of observations, is approximately normally distributed.
# Histogram of residuals
# The distribution appears symmetric, bell-shaped, and centered around zero. This
# visually supports the assumption of approximate normality of the residuals.
# Shapiro-Wilk Test
# The Shapiro-Wilk test (W = 0.99064, p-value < 2.2e-16) formally rejects the 
# null hypothesis of normality. However, with a large sample size (n = 5000),
# the test is extremely sensitive to even minor deviations. Therefore, the 
# result primarily reflects the sample size rather than a meaningful violation
# of the normality assumption
# Residuals vs Fitted
# Residuals are randomly scattered around zero with no clear pattern, indicating
# that the linearity assumption is satisfied. A few outliers (e.g., observations
# 13326, 16754, 9127) are present, but they do not appear to have symmetric 
# influence on the model.
# Scale-Location Plot
# Slight upward trend suggests mild heteroskedasticity at higher fitted values,
# indicating that the model is somewhat less accurate for ZIP codes with
# extreely high prevalence values.

pred_lm <- predict(lm_model, newdata = test)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

r2 <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

lm_rmse <- rmse(test$GHLTH_CrudePrev, pred_lm)
lm_mae  <- mae(test$GHLTH_CrudePrev, pred_lm)
lm_r2   <- r2(test$GHLTH_CrudePrev, pred_lm)

lm_rmse
lm_mae
lm_r2

# Saving LM results
results <- list()
results$lm <- tibble(Model = "Linear Regression",
                    RMSE = lm_rmse, MAE = lm_mae, R2 = lm_r2)
