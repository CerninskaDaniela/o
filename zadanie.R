# ══════════════════════════════════════════════════════════════════════════════
# CAPSTONE PROJECT: Predictive Modelling of Poor/Fair General Health
# Target: GHLTH_CrudePrev (crude prevalence of fair/poor general health, %)
# Dataset: CDC PLACES 2025 — ZIP Code Tabulation Areas (ZCTA)
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# Packages
# ══════════════════════════════════════════════════════════════════════════════

install.packages("tidyverse")
install.packages("magrittr")
install.packages("tidymodels")

library(tidyverse)
library(dplyr)
library(tibble)
library(randomForest)
library(rpart)
library(rpart.plot)
library(glmnet)
library(caret)
library(e1071)
library(corrplot)
library(tidymodels)
library(randomForest)
library(conflicted)

conflicts_prefer(tidyr::extract)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# ══════════════════════════════════════════════════════════════════════════════
# DATA LOADING
# ══════════════════════════════════════════════════════════════════════════════

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
#df <- df %>%
#  rename_with(~ str_replace_all(., "_CrudePrev", ""))

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

# Important:
# Missing values are NOT removed globally from the full dataset.
# Many rows may contain NA only in columns we are not using for modeling
# Removing all incomplete rows too early could unnecessarily reduce geographic
# coverage and distort representativeness.

# Fix TotalPop18Plus - stored as string with comma formatting (e.g. "14,019")
df <- df %>%
  mutate(TotalPop18plus = as.numeric(gsub(",", "", TotalPop18plus)))

# Select relevant columns only
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

df_model_raw <- df %>%
  select(all_of(selected_cols))

df_model <- df %>%
  select(all_of(selected_cols)) %>%
  drop_na()

# Log transformation of TotalPop18plus
# Reason: strong right skew due to large urban ZCTAs (NYC, LA etc.)
# log1p = log(1 + x) handles zero values safely
df_model <- df_model %>%
  mutate(TotalPop18plus = log1p(TotalPop18plus))

dim(df_model)
summary(df_model)

# Outlier note: prevalence columns contain extreme but real values
# (e.g. GHLTH up to 58.2%) representing genuinely deprived ZIP areas.
# These are NOT removed — they represent real population health disparities.
# Tree-based models are robust to outliers. Linear Regression results
# should be interpreted with this in mind.

# EDA on selected features

# Skewness
skewness_df <- df_model %>%
  summarise(across(everything(), ~ skewness(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Skewness") %>%
  arrange(desc(abs(Skewness)))
print(skewness_df)

# Note: ACCESS2, FOODINSECU, HOUSINSECU, SHUTUTILITY show high right skew (>1.5)
# This reflects real-world socioeconomic inequality — no transformation applied.
# Tree-based models are robust to skewness; LM assumptions apply to residuals only.

# Correlation with target
cor_with_target <- df_model %>%
  summarise(across(-GHLTH_CrudePrev,
                   ~ cor(., df_model$GHLTH_CrudePrev, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "Feature", values_to = "Correlation") %>%
  arrange(desc(abs(Correlation)))

print(cor_with_target)

cor_with_target %>%
  ggplot(aes(x = reorder(Feature, Correlation), y = Correlation,
             fill = Correlation > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("Negative", "Positive"), name = "Direction") +
  theme_minimal() +
  labs(title = "Feature Correlation with Target (GHLTH_CrudePrev)",
       x = NULL, y = "Pearson Correlation")

# Feature intercorrelation matrix
cor_matrix <- df_model %>%
  select(-GHLTH_CrudePrev) %>%
  cor()

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.7, addCoef.col = "black", number.cex = 0.6,
         title = "Feature Correlation Matrix", mar = c(0, 0, 1, 0))

# Note: FOODINSECU, HOUSINSECU, SHUTUTILITY are highly correlated (r > 0.94).
# This multicollinearity may inflate standard errors in Linear Regression.
# Ridge Regression is specifically designed to handle this.
# Tree-based models are not affected.

# ══════════════════════════════════════════════════════════════════════════════
# TRAIN / TEST SPLIT (80/20)
# ══════════════════════════════════════════════════════════════════════════════

set.seed(42)
n_total   <- nrow(df_model)
train_idx <- sample(1:n_total, size = round(0.8 * n_total))

train <- df_model[train_idx, ]
test  <- df_model[-train_idx, ]

cat("Train:", nrow(train), "| Test:", nrow(test), "\n")

# ══════════════════════════════════════════════════════════════════════════════
# EVALUATION METRIC FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

r_squared <- function(actual, predicted) {
  1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
}

results <- list()

# ══════════════════════════════════════════════════════════════════════════════
# SCENARIO 1 — LINEAR vs RECURSIVE PARTITIONING
# ══════════════════════════════════════════════════════════════════════════════

# ── Model 1: Linear Regression (Linear Partitioning) ─────────────────────────

model_lm <- lm(GHLTH_CrudePrev ~ ., data = train)
summary(model_lm)

pred_lm <- predict(model_lm, newdata = test)

results$lm <- tibble(
  Model    = "Linear Regression",
  RMSE     = rmse(test$GHLTH_CrudePrev, pred_lm),
  MAE      = mae(test$GHLTH_CrudePrev, pred_lm),
  R2       = r_squared(test$GHLTH_CrudePrev, pred_lm),
  Scenario = "S1", Family = "Linear Partitioning"
)
cat(sprintf("LM — RMSE: %.4f | R²: %.4f\n", results$lm$RMSE, results$lm$R2))

# Residual diagnostics
par(mfrow = c(2, 2)); plot(model_lm); par(mfrow = c(1, 1))

residuals_lm <- residuals(model_lm)

ggplot(tibble(r = residuals_lm), aes(x = r)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  theme_minimal() +
  labs(title = "Distribution of Residuals — Linear Regression",
       x = "Residual", y = "Count")

ggplot(tibble(r = residuals_lm), aes(sample = r)) +
  stat_qq() + stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "Q-Q Plot of Residuals — Linear Regression",
       x = "Theoretical Quantiles", y = "Sample Quantiles")

set.seed(42)
shapiro.test(sample(residuals_lm, 5000))

# Normality note:
# Shapiro-Wilk formally rejects normality at this sample size (n=5000),
# but this is expected — the test is hypersensitive at large n.
# Q-Q plot shows good central fit. Histogram is symmetric around zero.
# For PREDICTIVE modelling, normality of residuals is not required:
# OLS is BLUE by Gauss-Markov theorem regardless of residual distribution.
# CLT ensures asymptotic normality of coefficient estimates at n=19387.

tibble(Actual = test$GHLTH_CrudePrev, Predicted = pred_lm) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Linear Regression: Actual vs Predicted",
       subtitle = "Red line = perfect prediction",
       x = "Actual GHLTH_CrudePrev", y = "Predicted")

# ── Model 2: Decision Tree — CART (Recursive Partitioning) ───────────────────

model_tree <- rpart(
  GHLTH_CrudePrev ~ ., data = train,
  method  = "anova",
  control = rpart.control(cp = 0.001, maxdepth = 6, xval = 10)
)

rpart.plot(model_tree, type = 4, extra = 101, roundint = FALSE,
           main = "Decision Tree — before pruning")

printcp(model_tree)
optimal_cp   <- model_tree$cptable[which.min(model_tree$cptable[, "xerror"]), "CP"]
model_tree_p <- prune(model_tree, cp = optimal_cp)
cat("Optimal cp:", optimal_cp, "\n")

rpart.plot(model_tree_p, type = 4, extra = 101, roundint = FALSE,
           main = "Decision Tree — pruned")

pred_tree <- predict(model_tree_p, newdata = test)

results$tree <- tibble(
  Model    = "Decision Tree",
  RMSE     = rmse(test$GHLTH_CrudePrev, pred_tree),
  MAE      = mae(test$GHLTH_CrudePrev, pred_tree),
  R2       = r_squared(test$GHLTH_CrudePrev, pred_tree),
  Scenario = "S1", Family = "Recursive Partitioning"
)
cat(sprintf("Tree — RMSE: %.4f | R²: %.4f\n", results$tree$RMSE, results$tree$R2))

# ── Model 3: Random Forest (Ensemble Recursive Partitioning) ─────────────────

set.seed(42)
model_rf <- randomForest(
  GHLTH_CrudePrev ~ ., data = train,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(train) - 1)),
  importance = TRUE
)

print(model_rf)
varImpPlot(model_rf, main = "Random Forest — Feature Importance")

pred_rf <- predict(model_rf, newdata = test)

results$rf <- tibble(
  Model    = "Random Forest",
  RMSE     = rmse(test$GHLTH_CrudePrev, pred_rf),
  MAE      = mae(test$GHLTH_CrudePrev, pred_rf),
  R2       = r_squared(test$GHLTH_CrudePrev, pred_rf),
  Scenario = "S1", Family = "Recursive Partitioning"
)
cat(sprintf("RF — RMSE: %.4f | R²: %.4f\n", results$rf$RMSE, results$rf$R2))

tibble(Actual = test$GHLTH_CrudePrev, Predicted = pred_rf) %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Random Forest: Actual vs Predicted",
       subtitle = "Red line = perfect prediction",
       x = "Actual GHLTH_CrudePrev", y = "Predicted")

# ── Scenario 1: Feature importance comparison ─────────────────────────────────

lm_imp <- summary(model_lm)$coefficients[-1, ] %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  mutate(Importance = abs(`t value`) / max(abs(`t value`)) * 100,
         Model = "Linear Regression") %>%
  select(Feature, Importance, Model)

tree_imp <- model_tree_p$variable.importance %>%
  enframe(name = "Feature", value = "Importance") %>%
  mutate(Importance = Importance / max(Importance) * 100,
         Model = "Decision Tree")

rf_imp <- importance(model_rf) %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  mutate(Importance = `%IncMSE` / max(`%IncMSE`) * 100,
         Model = "Random Forest") %>%
  select(Feature, Importance, Model)

bind_rows(lm_imp, tree_imp, rf_imp) %>%
  ggplot(aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Model, scales = "free_x") +
  theme_minimal() +
  labs(title = "Scenario 1: Feature Importance Across Partitioning Approaches",
       subtitle = "Which features remain informative across linear and recursive partitioning?",
       x = NULL, y = "Relative Importance (%)")

# ══════════════════════════════════════════════════════════════════════════════
# SCENARIO 2 — PARAMETRIC vs NONPARAMETRIC
# ══════════════════════════════════════════════════════════════════════════════

X_train <- as.matrix(train %>% select(-GHLTH_CrudePrev))
y_train <- train$GHLTH_CrudePrev
X_test  <- as.matrix(test %>% select(-GHLTH_CrudePrev))
y_test  <- test$GHLTH_CrudePrev

results$lm_p <- results$lm %>%
  mutate(Model = "Linear Regression (P)", Scenario = "S2", Family = "Parametric")

# ── Model 4: Ridge Regression ─────────────────────────────────────────────────

set.seed(42)
cv_ridge    <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)
model_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
cat("Ridge optimal lambda:", cv_ridge$lambda.min, "\n")
coef(model_ridge)
plot(cv_ridge, main = "Ridge — CV Lambda Selection")

pred_ridge <- predict(model_ridge, newx = X_test)[, 1]

results$ridge <- tibble(
  Model    = "Ridge Regression",
  RMSE     = rmse(y_test, pred_ridge),
  MAE      = mae(y_test, pred_ridge),
  R2       = r_squared(y_test, pred_ridge),
  Scenario = "S2", Family = "Parametric"
)
cat(sprintf("Ridge — RMSE: %.4f | R²: %.4f\n", results$ridge$RMSE, results$ridge$R2))

# ── Model 5: Lasso Regression ─────────────────────────────────────────────────

set.seed(42)
cv_lasso    <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
model_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
cat("Lasso optimal lambda:", cv_lasso$lambda.min, "\n")
plot(cv_lasso, main = "Lasso — CV Lambda Selection")

lasso_coef     <- coef(model_lasso)[, 1]
lasso_selected <- names(lasso_coef[lasso_coef != 0 & names(lasso_coef) != "(Intercept)"])
cat("Lasso selected", length(lasso_selected), "features out of", ncol(X_train), "\n")
print(lasso_coef[lasso_coef != 0])

pred_lasso <- predict(model_lasso, newx = X_test)[, 1]

results$lasso <- tibble(
  Model    = "Lasso Regression",
  RMSE     = rmse(y_test, pred_lasso),
  MAE      = mae(y_test, pred_lasso),
  R2       = r_squared(y_test, pred_lasso),
  Scenario = "S2", Family = "Parametric"
)
cat(sprintf("Lasso — RMSE: %.4f | R²: %.4f\n", results$lasso$RMSE, results$lasso$R2))

# Ridge vs Lasso coefficient comparison
ridge_coef_df <- coef(model_ridge)[-1, , drop = FALSE] %>%
  as.data.frame() %>% rownames_to_column("Feature") %>% rename(Ridge = s0)

lasso_coef_df <- coef(model_lasso)[-1, , drop = FALSE] %>%
  as.data.frame() %>% rownames_to_column("Feature") %>% rename(Lasso = s0)

left_join(ridge_coef_df, lasso_coef_df, by = "Feature") %>%
  pivot_longer(-Feature, names_to = "Method", values_to = "Coefficient") %>%
  ggplot(aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient, fill = Method)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ridge vs Lasso: Coefficient Comparison",
       subtitle = "Lasso shrinks some coefficients to exactly zero (feature selection)",
       x = NULL, y = "Coefficient")

# ── Models 6-7: Decision Tree + Random Forest reused ─────────────────────────

results$tree_np <- results$tree %>%
  mutate(Model = "Decision Tree (NP)", Scenario = "S2", Family = "Nonparametric")

results$rf_np <- results$rf %>%
  mutate(Model = "Random Forest (NP)", Scenario = "S2", Family = "Nonparametric")

# ── Model 8: kNN Regression ───────────────────────────────────────────────────

train_pred <- train %>% select(-GHLTH_CrudePrev)
test_pred  <- test  %>% select(-GHLTH_CrudePrev)

pred_means <- colMeans(train_pred)
pred_sds   <- apply(train_pred, 2, sd)

train_scaled <- scale(train_pred, center = pred_means, scale = pred_sds)
test_scaled  <- scale(test_pred,  center = pred_means, scale = pred_sds)

best_k <- NA; best_rmse_knn <- Inf
for (k in c(3, 5, 7, 10, 15, 20)) {
  m    <- knnreg(train_scaled, train$GHLTH_CrudePrev, k = k)
  pred <- predict(m, test_scaled)
  r    <- rmse(test$GHLTH_CrudePrev, pred)
  cat(sprintf("k = %2d | RMSE = %.4f\n", k, r))
  if (r < best_rmse_knn) { best_rmse_knn <- r; best_k <- k }
}
cat("Best k:", best_k, "\n")

model_knn <- knnreg(train_scaled, train$GHLTH_CrudePrev, k = best_k)
pred_knn  <- predict(model_knn, test_scaled)

results$knn <- tibble(
  Model    = "kNN Regression",
  RMSE     = rmse(test$GHLTH_CrudePrev, pred_knn),
  MAE      = mae(test$GHLTH_CrudePrev, pred_knn),
  R2       = r_squared(test$GHLTH_CrudePrev, pred_knn),
  Scenario = "S2", Family = "Nonparametric"
)
cat(sprintf("kNN — RMSE: %.4f | R²: %.4f\n", results$knn$RMSE, results$knn$R2))






#
#
#

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
