install.packages('shinythemes')
library(shiny)
library(shinythemes)
library(ggplot2)
install.packages("rlang")
install.packages("recipes")
install.packages("parsnip")
install.packages("kknn")
install.packages("arm")
library(rlang)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(kknn)
library(dplyr)
library(rpart)
library(tibble)
library(corrplot)
library(randomForest)
library(tidyverse)
library(magrittr)
library(patchwork)
library(ggplot2)
library(e1071)
library(caret)
library(conflicted)
library(rsample)
library(glmnet)
library(arm)
conflicts_prefer(dplyr::filter) 
conflicts_prefer(tidyr::extract) 
conflicts_prefer(rpart::prune)
conflicts_prefer(dplyr::select)

set.seed(42)
TARGET <- "GHLTH_CrudePrev"
PREDICTORS <- c(
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

# Function to load and preprocess data
load_and_preprocess_data <- function() {
  # Try to load raw data
  data_path <- "PLACES__ZCTA_Data_(GIS_Friendly_Format),_2025_release_20260423.csv"
  
  if (!file.exists(data_path)) {
    stop("Dataset not found. Please ensure CSV file is in working directory.")
  }
  
  df_raw <- read_csv(data_path, show_col_types = FALSE)
  
  # Convert population columns from comma-formatted strings to numeric
  df_raw <- df_raw %>%
    mutate(across(c(TotalPopulation, TotalPop18plus), ~ {
      clean_val <- gsub("[^0-9]", "", .x)
      as.numeric(clean_val)
    }))
  
  # Create model dataset with selected variables
  df_model <- df_raw %>%
    select(all_of(c(TARGET, PREDICTORS))) %>%
    drop_na() %>%
    mutate(TotalPop18plus = log1p(TotalPop18plus))
  
  return(list(
    df_raw = df_raw,
    df_model = df_model
  ))
}

# Load data at app startup
tryCatch({
  data_list <- load_and_preprocess_data()
  df_raw <- data_list$df_raw
  df_model <- data_list$df_model
}, error = function(e) {
  # If data loading fails, create empty placeholders
  df_raw <- data.frame()
  df_model <- data.frame()
})

# Load precomputed results if available
load_model_results <- function() {
  if (file.exists("model_results.rds")) {
    readRDS("model_results.rds")
  } else {
    # Placeholder table with expected model structure
    data.frame(
      Model = c("Linear Regression", "Ridge Regression", "Lasso Regression",
                "Decision Tree", "Random Forest", "k-NN"),
      RMSE = NA_real_,
      MAE = NA_real_,
      R2 = NA_real_
    )
  }
}

load_predictions <- function() {
  if (file.exists("predictions_df.rds")) {
    readRDS("predictions_df.rds")
  } else {
    NULL
  }
}

load_rf_importance <- function() {
  if (file.exists("rf_importance.rds")) {
    readRDS("rf_importance.rds")
  } else {
    NULL
  }
}

load_lasso_features <- function() {
  if (file.exists("lasso_selected_features.rds")) {
    readRDS("lasso_selected_features.rds")
  } else {
    NULL
  }
}

# Load results at app startup
model_results <- load_model_results()
predictions_df <- load_predictions()
rf_importance <- load_rf_importance()
lasso_features <- load_lasso_features()

data_available <- nrow(df_model) > 0
models_available <- !all(is.na(model_results$RMSE))
predictions_available <- !is.null(predictions_df)
importance_available <- !is.null(rf_importance)
lasso_available <- !is.null(lasso_features)


df <- read_csv("PLACES__ZCTA_Data_(GIS_Friendly_Format),_2025_release_20260423.csv")
df <- df %>%
  mutate(across(c(TotalPopulation, TotalPop18plus), ~ {
    clean_val <- gsub("[^0-9]", "", .x)
    as.numeric(clean_val)
  }))
ci_cols <- names(df)[grep("_Crude95CI", names(df))]

for (col in ci_cols) {
  width_name <- paste0(col, "_width")
  high_name <- paste0(col, "_high")
  
  clean_vec <- str_remove_all(df[[col]], "[()\\s]")
  split_matrix <- str_split_fixed(clean_vec, ",", 2)
  df[[width_name]] <- as.numeric(as.numeric(split_matrix[, 2]) - as.numeric(split_matrix[, 1]))
  df[[high_name]] <- as.numeric(split_matrix[, 2])
  
  df <- df %>%
    relocate(all_of(width_name), .after = !!sym(col)) %>%
    select(-!!sym(col))
}
cols_crude_char <- df %>%
  select(ends_with("CrudePrev") & where(is.character)) %>%
  colnames()
df <- df %>%
  mutate(across(all_of(cols_crude_char), ~ {
    as.numeric(gsub(",", ".", .x))
  }))
cols_crude_num <- df %>%
  select(ends_with("CrudePrev") & where(is.numeric)) %>%
  colnames()
walk(cols_crude_num, function(col_name) {
  high_name <- paste0(str_remove(col_name, "CrudePrev"), "Crude95CI_high")
  
  
  if (high_name %in% colnames(df)) {
    df <<- df %>%
      mutate(!!sym(col_name) := if_else(
        !is.na(!!sym(col_name)) & !is.na(!!sym(high_name)) & !!sym(col_name) > !!sym(high_name),
        !!sym(col_name) / 10,
        !!sym(col_name)
      ))
  }
})
df <- df %>%
  select(-ends_with("high"))
df <- df %>%
  separate_wider_regex(
    cols = Geolocation,
    patterns = c(
      ".*\\(",                 
      Longitude = "-?\\d+\\.\\d+", 
      " ",                     
      Latitude = "-?\\d+\\.\\d+",
      "\\)"                   
    ),
    too_few = "align_start"
  ) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )
df <- df %>%
  rename(
    Insurance_lack_m = ACCESS2_CrudePrev,
    Insurance_lack_w = ACCESS2_Crude95CI_width,
    Arthritis_m = ARTHRITIS_CrudePrev,
    Arthritis_w = ARTHRITIS_Crude95CI_width,
    Drinking_m = BINGE_CrudePrev,
    Drinking_w = BINGE_Crude95CI_width,
    HBP_m = BPHIGH_CrudePrev,
    HBP_w = BPHIGH_Crude95CI_width,
    HBP_med_m = BPMED_CrudePrev,
    HBP_med_w = BPMED_Crude95CI_width,
    Cancer_m = CANCER_CrudePrev,
    Cancer_w = CANCER_Crude95CI_width,
    Asthma_m = CASTHMA_CrudePrev,
    Asthma_w = CASTHMA_Crude95CI_width,
    CHD_m = CHD_CrudePrev,
    CHD_w = CHD_Crude95CI_width,
    Checkup_m = CHECKUP_CrudePrev,
    Checkup_w = CHECKUP_Crude95CI_width,
    Chol_screen_m = CHOLSCREEN_CrudePrev,
    Chol_screen_w = CHOLSCREEN_Crude95CI_width,
    Colon_screen_m = COLON_SCREEN_CrudePrev,
    Colon_screen_w = COLON_SCREEN_Crude95CI_width,
    COPD_m = COPD_CrudePrev,
    COPD_w = COPD_Crude95CI_width,
    Smoking_m = CSMOKING_CrudePrev,
    Smoking_w = CSMOKING_Crude95CI_width,
    Dental_m = DENTAL_CrudePrev,
    Dental_w = DENTAL_Crude95CI_width,
    Depression_m = DEPRESSION_CrudePrev,
    Depression_w = DEPRESSION_Crude95CI_width,
    Diabetes_m = DIABETES_CrudePrev,
    Diabetes_w = DIABETES_Crude95CI_width,
    General_hlth_m = GHLTH_CrudePrev,
    General_hlth_w = GHLTH_Crude95CI_width,
    High_chol_m = HIGHCHOL_CrudePrev,
    High_chol_w = HIGHCHOL_Crude95CI_width,
    No_PA_m = LPA_CrudePrev,
    No_PA_w = LPA_Crude95CI_width,
    Mammography_m = MAMMOUSE_CrudePrev,
    Mammography_w = MAMMOUSE_Crude95CI_width,
    Mental_distress_m = MHLTH_CrudePrev,
    Mental_distress_w = MHLTH_Crude95CI_width,
    Obesity_m = OBESITY_CrudePrev,
    Obesity_w = OBESITY_Crude95CI_width,
    Physical_distress_m = PHLTH_CrudePrev,
    Physical_distress_w = PHLTH_Crude95CI_width,
    Short_sleep_m = SLEEP_CrudePrev,
    Short_sleep_w = SLEEP_Crude95CI_width,
    Stroke_m = STROKE_CrudePrev,
    Stroke_w = STROKE_Crude95CI_width,
    Teeth_lost_m = TEETHLOST_CrudePrev,
    Teeth_lost_w = TEETHLOST_Crude95CI_width,
    Hearing_m = HEARING_CrudePrev,
    Hearing_w = HEARING_Crude95CI_width,
    Vision_m = VISION_CrudePrev,
    Vision_w = VISION_Crude95CI_width,
    Cognition_m = COGNITION_CrudePrev,
    Cognition_w = COGNITION_Crude95CI_width,
    Mobility_m = MOBILITY_CrudePrev,
    Mobility_w = MOBILITY_Crude95CI_width,
    Selfcare_m = SELFCARE_CrudePrev,
    Selfcare_w = SELFCARE_Crude95CI_width,
    Indep_living_m = INDEPLIVE_CrudePrev,
    Indep_living_w = INDEPLIVE_Crude95CI_width,
    Disability_m = DISABILITY_CrudePrev,
    Disability_w = DISABILITY_Crude95CI_width,
    Loneliness_m = LONELINESS_CrudePrev,
    Loneliness_w = LONELINESS_Crude95CI_width,
    Food_stamp_m = FOODSTAMP_CrudePrev,
    Food_stamp_w = FOODSTAMP_Crude95CI_width,
    Food_insecure_m = FOODINSECU_CrudePrev,
    Food_insecure_w = FOODINSECU_Crude95CI_width,
    House_insecure_m = HOUSINSECU_CrudePrev,
    House_insecure_w = HOUSINSECU_Crude95CI_width,
    Utility_threat_m = SHUTUTILITY_CrudePrev,
    Utility_threat_w = SHUTUTILITY_Crude95CI_width,
    Transport_lack_m = LACKTRPT_CrudePrev,
    Transport_lack_w = LACKTRPT_Crude95CI_width,
    Support_lack_m = EMOTIONSPT_CrudePrev,
    Support_lack_w = EMOTIONSPT_Crude95CI_width
  )
all_columns <- df %>% 
  select(-ZCTA5) %>% 
  names()

df_model <- df %>%
  select(all_of(all_columns)) %>%
  na.omit()



ui <- navbarPage(
  title = "OZNAL",
  
  theme = bslib::bs_theme(version = 5, preset = "flatly"),
  
  tabPanel(
    "Overview",
    fluidRow(
      column(
        12,
        h2("General Health Status Prediction Model"),
        hr()
      )
    ),
    fluidRow(
      column(
        6,
        h4("Project Goal"),
        p("Predict the crude prevalence of fair or poor general health outcomes (GHLTH_CrudePrev)
          across US ZIP Code Tabulation Areas (ZCTAs) using behavioral and socioeconomic
          determinants of health."),
        h4("Why This Matters"),
        p("Health outcomes are influenced by more than just individual behavior. This project
          explores how access to care, substance use, physical activity, sleep quality, obesity,
          food insecurity, housing insecurity, and utility burden collectively predict self-reported
          poor or fair health status across communities."),
        h4("Methodological Note"),
        p(strong("Avoiding Circular Reasoning:"), "Direct disease outcomes (stroke, cancer,
          arthritis, high blood pressure, COPD, depression, etc.) are explicitly excluded as
          predictors. We focus only on behavioral risk factors and socioeconomic determinants.")
      ),
      column(
        6,
        h4("Target Variable"),
        div(
          style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px;",
          p(strong("GHLTH_CrudePrev"), " (Crude Prevalence of Fair/Poor General Health)"),
          p("Percentage of adults aged 18+ who report their general health as fair or poor."),
          p("Scale: 0 - 100 %"),
          p(strong("Interpretation:"), "Higher values indicate worse self-reported health status.")
        )
      )
    ),
    fluidRow(
      column(
        12,
        h4("Selected Predictors (Behavioral & Socioeconomic)"),
        div(
          style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px;",
          fluidRow(
            column(
              6,
              h5("Healthcare & Substance Use"),
              tags$ul(
                tags$li(strong("Insurance_lack_m"), ": Limited access to places for physical activity"),
                tags$li(strong("Drinking_m"), ": Binge drinking prevalence"),
                tags$li(strong("Smoking_m"), ": Current smoking prevalence")
              )
            ),
            column(
              6,
              h5("Health Behaviors & Living Conditions"),
              tags$ul(
                tags$li(strong("No_PA_m"), ": Lack of leisure-time physical activity"),
                tags$li(strong("Short_sleep_m"), ": Insufficient sleep"),
                tags$li(strong("Obesity_m"), ": Obesity prevalence")
              )
            )
          ),
          fluidRow(
            column(
              6,
              h5("Socioeconomic Determinants"),
              tags$ul(
                tags$li(strong("Food_insecure_m"), ": Food insecurity"),
                tags$li(strong("House_insecure_m"), ": Housing insecurity")
              )
            ),
            column(
              6,
              h5("Utility Access"),
              tags$ul(
                tags$li(strong("Utility_threat_m"), ": Shut-off utility burden"),
                tags$li(strong("TotalPop18plus"), ": Adult population (log-scaled)")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        hr(),
        p(em("Data Source: PLACES: Local Data for Better Health, CDC, 2025 Release"),
          style = "color: #666; font-size: 0.9em;")
      )
    )
  ),
  tabPanel(
    "Data Explorer",
    fluidRow(
      column(
        12,
        h3("Dataset Overview & Variable Distribution"),
        if (!data_available) {
          div(
            style = "background-color: #ffe6e6; padding: 15px; border-radius: 5px; margin: 10px 0;",
            p(strong("⚠ Data not loaded."), "Please check that the CSV file is in the working directory.")
          )
        }
      )
    ),
    fluidRow(
      column(
        4,
        h4("Dataset Summary"),
        br(),
        if (data_available) {
          div(
            p(strong("Raw Data Rows:"), nrow(df_raw)),
            p(strong("After Preprocessing:"), nrow(df_model)),
            p(strong("Rows Removed (NA):"), nrow(df_raw) - nrow(df_model)),
            p(strong("Features (Predictors):"), length(all_columns)),
            hr(),
            h5("Summary Statistics"),
            DT::dataTableOutput("summary_stats_table")
          )
        } else {
          p("Data not available.")
        }
      ),
      column(
        8,
        h4("Variable Distribution"),
        if (data_available) {
          div(
            selectInput(
              "selected_variable",
              "Select Variable for Histogram:",
              choices = c(TARGET, all_columns),
              selected = TARGET
            ),
            plotOutput("histogram_plot", height = "400px")
          )
        } else {
          p("Data not available.")
        }
      )
    ),
    fluidRow(
      column(
        12,
        hr(),
        h4("Predictor vs Target Relationship"),
        if (data_available) {
          div(
            selectInput(
              "selected_predictor",
              "Select Predictor for Scatterplot:",
              choices = all_columns,
              selected = all_columns[1]
            ),
            plotOutput("scatterplot", height = "400px")
          )
        } else {
          p("Data not available.")
        }
      )
    )
  ),
  
  # ═════════════════════════════════════════════════════════════════════════════
  # TAB 3: MODEL COMPARISON
  # ═════════════════════════════════════════════════════════════════════════════
  
  tabPanel(
    "Model Comparison",
    fluidRow(
      column(
        12,
        h3("Model Performance Summary"),
        if (!models_available) {
          div(
            style = "background-color: #ffe6e6; padding: 15px; border-radius: 5px; margin: 10px 0;",
            p(strong("ℹ Model results not loaded."),
              "Please export model_results.rds from final_project_pipeline.R."),
            p("For now, showing expected model structure.")
          )
        }
      )
    ),
    fluidRow(
      column(
        12,
        h4("Results Table"),
        DT::dataTableOutput("model_results_table")
      )
    ),
    fluidRow(
      column(
        12,
        hr(),
        h4("Model Comparison Chart"),
        div(
          selectInput(
            "comparison_metric",
            "Compare Models By:",
            choices = c("RMSE", "MAE", "R2"),
            selected = "RMSE"
          ),
          plotOutput("comparison_chart", height = "400px")
        )
      )
    ),
    fluidRow(
      column(
        12,
        hr(),
        h4("Interpretation"),
        p(strong("RMSE (Root Mean Squared Error):"), "Average prediction error. Lower is better."),
        p(strong("MAE (Mean Absolute Error):"), "Average absolute deviation. Lower is better."),
        p(strong("R² (R-squared):"), "Proportion of variance explained. Higher is better. Range: 0-1.")
      )
    )
  ),
  
  # ═════════════════════════════════════════════════════════════════════════════
  # TAB 4: PREDICTION DIAGNOSTICS
  # ═════════════════════════════════════════════════════════════════════════════
  
  tabPanel(
    "Prediction Diagnostics",
    fluidRow(
      column(
        12,
        h3("Model Predictions & Residual Analysis"),
        if (!predictions_available) {
          div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;",
            p(strong("ℹ Prediction data not available."),
              "To enable this tab:"),
            tags$ol(
              tags$li("Run final_project_pipeline.R to completion"),
              tags$li("Export predictions: ",
                      code("saveRDS(data.frame(actual = test_data$GHLTH_CrudePrev, 
                                          predicted = pred_rf, 
                                          residuals = test_data$GHLTH_CrudePrev - pred_rf),
                      'predictions_df.rds')")),
              tags$li("Restart this Shiny app")
            ),
            p(em("(Using Random Forest predictions in example above)"))
          )
        }
      )
    ),
    if (predictions_available) {
      list(
        fluidRow(
          column(
            6,
            h4("Actual vs Predicted"),
            plotOutput("actual_vs_predicted_plot", height = "400px")
          ),
          column(
            6,
            h4("Residuals Distribution"),
            plotOutput("residuals_histogram", height = "400px")
          )
        ),
        fluidRow(
          column(
            12,
            hr(),
            h4("Prediction Summary"),
            DT::dataTableOutput("prediction_summary_table")
          )
        )
      )
    }
  ),
  
  # ═════════════════════════════════════════════════════════════════════════════
  # TAB 5: FEATURE IMPORTANCE
  # ═════════════════════════════════════════════════════════════════════════════
  
  tabPanel(
    "Feature Importance",
    fluidRow(
      column(
        12,
        h3("Model Feature Importance & Selection"),
        if (!importance_available && !lasso_available) {
          div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px 0;",
            p(strong("ℹ Feature importance data not available."),
              "To enable this tab:"),
            tags$ol(
              tags$li("Run final_project_pipeline.R to completion"),
              tags$li("Export Random Forest importance: ",
                      code("saveRDS(data.frame(Variable = rownames(model_rf$importance),
                                       Importance = model_rf$importance[, '%IncMSE']),
                      'rf_importance.rds')")),
              tags$li("Export Lasso selected features: ",
                      code("saveRDS(lasso_selected, 'lasso_selected_features.rds')")),
              tags$li("Restart this Shiny app")
            )
          )
        }
      )
    ),
    if (importance_available) {
      fluidRow(
        column(
          12,
          h4("Random Forest: Variable Importance"),
          p("Mean Decrease in MSE - shows how much each feature reduces prediction error."),
          plotOutput("importance_plot", height = "400px")
        )
      )
    },
    if (lasso_available) {
      fluidRow(
        column(
          12,
          hr(),
          h4("Lasso: Selected Features"),
          p("Features with non-zero coefficients in L1-regularized linear model."),
          DT::dataTableOutput("lasso_features_table")
        )
      )
    },
    if (!importance_available && !lasso_available) {
      fluidRow(
        column(
          12,
          br(),
          p("Waiting for feature importance exports...")
        )
      )
    }
  ),
  tabPanel(
    "Modeling",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Global Controls"),
        selectInput("target", "Target (Y):", choices = all_columns, selected = "Stroke_m"),
        selectizeInput("feature", "Predictors (X):", 
                       choices = all_columns, 
                       selected = c("Smoking_m", "Drinking_m"), 
                       multiple = TRUE,
                       options = list(plugins = list('remove_button'))),
        hr(),
        selectInput("model", "Model:", choices = c("Linear regression", "Ridge regression", "Lasso regression", "k-NN", "Random forest", "Gradient boosting", "MARS", "Bayesian regression", "SVR", "Decision tree"), selected = "Linear regression"),
        hr(),
        numericInput(
          inputId = "num_input", label = "Select a Value:", value = 1000,
          min = 100, max = nrow(df_model), step = 1
        )
      ),
      
      mainPanel(
        width = 9,
        conditionalPanel(
          condition = "input.model == 'Linear regression'",
          h3("Linear Regression"),
          selectInput(
            "lr_plot_choice",
            "Select plot:",
            choices = c("all", "residual", "qq", "residual3", "residual4"),
            selected = "all"
          ),
          plotOutput("p_lr"),
          selectInput(
            "lr_text_choice",
            "Select text output:",
            choices = c("metrics", "summary"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_lr")
        ),
        conditionalPanel(
          condition = "input.model == 'k-NN'",
          h3("k-Nearest Neighbours"),
          sliderInput("k", "Number of k:", 1, 20, 1),
          selectInput(
            "knn_plot_choice",
            "Select plot:",
            choices = c("observed vs. predicted", "k vs. error", "smoothness"),
            selected = "observed vs. predicted"
          ),
          plotOutput("p_knn"),
          selectInput(
            "knn_text_choice",
            "Select text output:",
            choices = c("metrics", "importance"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_knn")
        ),
        conditionalPanel(
          condition = "input.model == 'Random forest'",
          h3("Random forest"),
          sliderInput("ntree", "Number of trees:", 1, 3000, 50, 1),
          sliderInput("mtry", "Mtry:", 1, 5, 1, 1),
          sliderInput("nodesize", "Nodesize:", 1, 20, 5, 1),
          sliderInput("maxnodes", "Maxnodes:", 1, 20, 5, 1),
          checkboxInput("replace_rf", "Use bootstrap sampling?", value = FALSE),
          selectInput(
            "rf_plot_choice",
            "Select plot:",
            choices = c("observed vs. predicted", "residual", "variable importance"),
            selected = "observed vs. predicted"
          ),
          plotOutput("p_rf"),
          selectInput(
            "rf_text_choice",
            "Select plot:",
            choices = c("metrics", "importance"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_rf")
        ),
        conditionalPanel(
          condition = "input.model == 'Lasso regression'",
          h3("LASSO Regression"),
          selectInput(
            "lsr_plot_choice",
            "Select plot:",
            choices = c("observed vs. predicted"),
            selected = "observed vs. predicted"
          ),
          plotOutput("p_lsr"),
          selectInput(
            "lsr_text_choice",
            "Select text output:",
            choices = c("metrics"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_lsr")
        ),
        conditionalPanel(
          condition = "input.model == 'Ridge regression'",
          h3("Ridge Regression"),
          selectInput(
            "rr_plot_choice",
            "Select plot:",
            choices = c("observed vs. predicted", "plot"),
            selected = "observed vs. predicted"
          ),
          plotOutput("p_rr"),
          selectInput(
            "rr_text_choice",
            "Select text output:",
            choices = c("metrics"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_rr")
        ),
        conditionalPanel(
          condition = "input.model == 'Bayesian regression'",
          h3("Bayesian regression"),
          selectInput(
            "br_plot_choice",
            "Select plot:",
            choices = c("observed vs. predicted"),
            selected = "observed vs. predicted"
          ),
          plotOutput("p_br"),
          selectInput(
            "br_text_choice",
            "Select text output:",
            choices = c("metrics", "importance"),
            selected = "metrics"
          ),
          verbatimTextOutput("s_br")
        ),
        conditionalPanel(
          condition = "input.model == 'Gradient boosting'",
          h3("Ridge regression"),
          plotOutput("p_gb"),
          verbatimTextOutput("s_gb")
        ),
        conditionalPanel(
          condition = "input.model == 'SVR'",
          h3("Support vector regression"),
          plotOutput("p_svr"),
          verbatimTextOutput("s_svr")
        ),
        conditionalPanel(
          condition = "input.model == 'Decision tree'",
          h3("Decision tree"),
          plotOutput("p_mars"),
          verbatimTextOutput("s_mars")
        ),
        conditionalPanel(
          condition = "input.model == 'MARS'",
          h3("Multivariate adaptive regression splines"),
          plotOutput("p_mars"),
          verbatimTextOutput("s_mars")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  sampled_df <- reactive({
    req(input$num_input)
    
    set.seed(42) 
    
    df_model %>%
      slice_sample(n = input$num_input)
  })
  
  split_data <- reactive({
    data <- sampled_df()
    
    set.seed(42)
    train_idx <- sample(1:nrow(data), 0.8 * nrow(data))
    
    list(
      train = data[train_idx, ],
      test  = data[-train_idx, ]
    )
  })
  
  data_engine <- reactive({
    req(input$target, input$feature)
    df_model
  })
  
  observe({
    req(input$num_input)
    max_rows <- nrow(df_model)
    if (input$num_input > max_rows) {
      updateNumericInput(session, "num_input", value = max_rows)
    }
    if (input$num_input < 100) {
      updateNumericInput(session, "num_input", value = 100)
    }
  })
  # ---------- Linear regression ---------------
  output$s_lr <- renderPrint({
    
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    lr_model <- lm(form, data = current_train)
    
    
    if (input$lr_text_choice == "metrics") {
      lr_preds <- predict(lr_model, current_test)
      lr_performance <- postResample(pred = lr_preds, obs = current_test[[input$target]])
      print(lr_performance)
    }
    else if (input$lr_text_choice == "summary") {
      print(summary(lr_model))
    }
    
  })
  
  output$p_lr <- renderPlot({
    current_train <- as.data.frame(split_data()$train)
    current_test  <- as.data.frame(split_data()$test)
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    if (input$lr_plot_choice == "all") {
      par(mfrow = c(2, 2))
      plot(lm(form, data = current_train))
      par(mfrow = c(1, 1))
    }
    else if (input$lr_plot_choice == "residual"){
      plot(lm(form, data = current_train), 1)
    }
    else if (input$lr_plot_choice == "qq"){
      plot(lm(form, data = current_train), 2)
    }
    else if (input$lr_plot_choice == "residual3"){
      plot(lm(form, data = current_train), 3)
    }
    else if (input$lr_plot_choice == "residual4"){
      plot(lm(form, data = current_train), 4)
    }
  })
  
  # ----------- KNN --------------
  output$s_knn <- renderPrint({
    current_train <- as.data.frame(split_data()$train)
    current_test  <- as.data.frame(split_data()$test)
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    knn_model <- train(
      form, 
      data = current_train, 
      method = "knn", 
      tuneGrid = data.frame(k = input$k),
      preProcess = c("center", "scale")
    )
    
    if (input$knn_text_choice == "metrics") {
      knn_preds <- predict(knn_model, current_test)
      knn_performance <- postResample(pred = knn_preds, obs = current_test[[input$target]])
      print(knn_performance)
    }
    else if(input$knn_text_choice == "importance"){
      importance_knn <- varImp(knn_model)
      print(importance_knn)
    }
    
  })
  output$p_knn <- renderPlot({
    current_train <- as.data.frame(split_data()$train)
    current_test  <- as.data.frame(split_data()$test)
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    knn_model <- train(
      form, 
      data = current_train, 
      method = "knn", 
      tuneGrid = data.frame(k = input$k),
      preProcess = c("center", "scale")
    )
    
    knn_preds <- predict(knn_model, current_test)
    knn_performance <- postResample(pred = knn_preds, obs = current_test[[input$target]])
    
    if (input$knn_plot_choice == "observed vs. predicted") {
      plot_data <- data.frame(
        observed = current_test[[input$target]],
        predicted = knn_preds
      )
      
      ggplot(plot_data, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        
        labs(title = "Observed vs. Predicted: k-NN Regression",
             x = "Observed Values",
             y = "Predicted Values") +
        theme_minimal()
    }
    
  })
  
  # ----------- Random forest ------------
  output$s_rf <- renderPrint({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    rf_model <- randomForest(
      form, 
      data = current_train,
      ntree = input$ntree, 
      mtry = input$mtry, 
      nodesize = input$nodesize,
      maxnodes = input$maxnodes,
      replace = input$replace_rf,
      importance = TRUE,
      na.action = na.omit
    )
    
    if (input$rf_text_choice == "metrics") {
      rf_preds <- predict(rf_model, current_test)
      rf_performance <- postResample(pred = rf_preds, obs = current_test[[input$target]])
      print(rf_performance)
    }
    else if (input$rf_text_choice == "importance"){
      importance(rf_model)
    }
    
  })
  output$p_rf <- renderPlot({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    rf_model <- randomForest(
      form, 
      data = current_train,
      ntree = input$ntree, 
      mtry = input$mtry, 
      nodesize = input$nodesize,
      maxnodes = input$maxnodes,
      replace = input$replace_rf,
      importance = TRUE,
      na.action = na.omit
    )
    rf_preds <- predict(rf_model, current_test)
    rf_performance <- postResample(pred = rf_preds, obs = current_test[[input$target]])
    
    if (input$rf_plot_choice == "observed vs. predicted") {
      plot_data <- data.frame(
        observed = current_test[[input$target]],
        predicted = rf_preds
      )
      
      ggplot(plot_data, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        
        labs(title = "Observed vs. Predicted: Random Forest",
             x = "Observed Values",
             y = "Predicted Values") +
        theme_minimal()
    }
    else if (input$rf_plot_choice == "variable importance") {
      importance_df <- as.data.frame(importance(rf_model))
      varImpPlot(rf_model, main = "Random Forest: Variable Importance")
    }
    else if (input$rf_plot_choice == "residual") {
      varImpPlot(rf_model, main = "Variable Importance")
    }
    
  })
  
  # ----------- Ridge regression --------------
  
  
  output$s_rr <- renderPrint({
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    current_train <- split_data()$train
    current_test  <- split_data()$test
    x_train <- model.matrix(form, current_train)[,-1]
    y_train <- current_train[[input$target]]
    
    x_test <- model.matrix(form, current_test)[,-1]
    y_test <- current_test[[input$target]]
    
    rr_model <- cv.glmnet(
      x_train,
      y_train,
      alpha = 0,
      standardize = TRUE
    )
    best_lambda <- rr_model$lambda.min
    
    
    if (input$rr_text_choice == "metrics") {
      rr_preds <- predict(rr_model, s=best_lambda, x_test)
      rr_preds <- as.numeric(rr_preds)
      rr_performance <- postResample(pred = rr_preds,  obs = y_test)
      print(rr_performance)
    }
    
  })
  output$p_rr <- renderPlot({
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    current_train <- split_data()$train
    current_test  <- split_data()$test
    x_train <- model.matrix(form, current_train)[,-1]
    y_train <- current_train[[input$target]]
    
    x_test <- model.matrix(form, current_test)[,-1]
    y_test <- current_test[[input$target]]
    
    rr_model <- cv.glmnet(
      x_train,
      y_train,
      alpha = 0,
      standardize = TRUE
    )
    best_lambda <- rr_model$lambda.min
    
    rr_preds <- predict(rr_model, s=best_lambda, newx = x_test)
    rr_preds <- as.numeric(rr_preds)
    rr_performance <- postResample(pred = rr_preds,  obs = y_test)
    
    if (input$rr_plot_choice == "observed vs. predicted") {
      plot_data <- data.frame(
        observed = y_test,
        predicted = rr_preds
      )
      
      ggplot(plot_data, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        
        labs(title = "Observed vs. Predicted: Ridge Regression",
             x = "Observed Values",
             y = "Predicted Values") +
        theme_minimal()
    }
    else if (input$rr_plot_choice == "plot") {
      plot(rr_model)
    }
    
  })
  
  # ------------ LASSO regression ----------------
  output$s_lsr <- renderPrint({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    lsr_model <- train(
      form, 
      data = current_train, 
      method = "knn", 
      tuneGrid = data.frame(k = input$k),
      preProcess = c("center", "scale")
    )
    
    if (input$lsr_text_choice == "metrics") {
      lsr_preds <- predict(lsr_model, current_test)
      lsr_performance <- postResample(pred = lsr_preds, obs = current_test[[input$target]])
      print(lsr_performance)
    }
    else if(input$lsr_text_choice == "importance"){
      importance_lsr <- varImp(lsr_model)
      print(importance_lsr)
    }
    
  })
  output$p_lsr <- renderPlot({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    lsr_model <- train(
      form, 
      data = current_train, 
      method = "knn", 
      tuneGrid = data.frame(k = input$k),
      preProcess = c("center", "scale")
    )
    
    lsr_preds <- predict(lsr_model, current_test)
    lsr_performance <- postResample(pred = lsr_preds, obs = current_test[[input$target]])
    
    if (input$lsr_plot_choice == "observed vs. predicted") {
      plot_data <- data.frame(
        observed = current_test[[input$target]],
        predicted = lsr_preds
      )
      
      ggplot(plot_data, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        
        labs(title = "Observed vs. Predicted: k-NN Regression",
             x = "Observed Values",
             y = "Predicted Values") +
        theme_minimal()
    }
    
  })
  # ------------------- Bayesian regression -----------------------
  output$s_br <- renderPrint({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    br_model <- bayesglm(
      form,
      data = current_train,
      family = gaussian()
    )
    
    if (input$br_text_choice == "metrics") {
      br_preds <- predict(br_model, current_test)
      br_performance <- postResample(pred = br_preds, obs = current_test[[input$target]])
      print(br_performance)
    }
    else if(input$br_text_choice == "importance"){
      importance_br <- varImp(br_model)
      print(importance_br)
    }
    else if (input$br_text_choice == "summary"){
      summary(bayes_model)
    }
    
  })
  output$p_br <- renderPlot({
    current_train <- split_data()$train
    current_test  <- split_data()$test
    form <- as.formula(paste(input$target, "~", paste(input$feature, collapse = "+")))
    
    br_model <- bayesglm(
      form,
      data = current_train,
      family = gaussian()
    )
    
    br_preds <- predict(br_model, current_test)
    br_performance <- postResample(pred = br_preds, obs = current_test[[input$target]])
    
    if (input$lsr_plot_choice == "observed vs. predicted") {
      plot_data <- data.frame(
        observed = current_test[[input$target]],
        predicted = br_preds
      )
      
      ggplot(plot_data, aes(x = observed, y = predicted)) +
        geom_point(alpha = 0.5, color = "blue") +
        
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
        
        labs(title = "Observed vs. Predicted: Bayes Regression",
             x = "Observed Values",
             y = "Predicted Values") +
        theme_minimal()
    }
    
  })
  

  # ───────────────────────────────────────────────────────────────────────────
  # TAB 2: DATA EXPLORER - Summary Statistics Table
  # ───────────────────────────────────────────────────────────────────────────
  
  output$summary_stats_table <- DT::renderDataTable({
    if (!data_available) return(NULL)
    
    summary_df <- df_model %>%
      summarise(across(everything(), list(
        Min = ~ min(., na.rm = TRUE),
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE)
      ))) %>%
      pivot_longer(everything(), names_to = "temp", values_to = "value") %>%
      extract(temp, into = c("Variable", "Stat"), regex = "(.*)_(.*)") %>%
      pivot_wider(names_from = Stat, values_from = value) %>%
      mutate(across(where(is.numeric), ~ round(., 3)))
    
    DT::datatable(
      summary_df,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 2: DATA EXPLORER - Histogram
  # ───────────────────────────────────────────────────────────────────────────
  
  output$histogram_plot <- renderPlot({
    if (!data_available) return(NULL)
    
    var_name <- input$selected_variable
    df_model %>%
      ggplot(aes(x = .data[[var_name]])) +
      geom_histogram(fill = "steelblue", color = "darkblue", bins = 30) +
      labs(
        title = paste("Distribution of", var_name),
        x = var_name,
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 2: DATA EXPLORER - Scatterplot
  # ───────────────────────────────────────────────────────────────────────────
  
  output$scatterplot <- renderPlot({
    if (!data_available) return(NULL)
    
    pred_name <- input$selected_predictor
    df_model %>%
      ggplot(aes(x = .data[[pred_name]], y = .data[[TARGET]])) +
      geom_point(alpha = 0.5, color = "steelblue", size = 1.5) +
      geom_smooth(method = "loess", color = "red", fill = "red", alpha = 0.2, se = TRUE) +
      labs(
        title = paste(TARGET, "vs", pred_name),
        x = pred_name,
        y = TARGET
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 3: MODEL COMPARISON - Results Table
  # ───────────────────────────────────────────────────────────────────────────
  
  output$model_results_table <- DT::renderDataTable({
    results_display <- model_results %>%
      mutate(across(where(is.numeric), ~ round(., 4)))
    
    DT::datatable(
      results_display,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 3: MODEL COMPARISON - Bar Chart
  # ───────────────────────────────────────────────────────────────────────────
  
  output$comparison_chart <- renderPlot({
    if (all(is.na(model_results[[input$comparison_metric]]))) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Model results not available.\nExport model_results.rds from final_project_pipeline.R",
           cex = 1.2, col = "gray")
      return()
    }
    
    metric_col <- input$comparison_metric
    
    model_results %>%
      arrange(!!sym(metric_col)) %>%
      ggplot(aes(x = reorder(Model, !!sym(metric_col)), y = !!sym(metric_col))) +
      geom_col(fill = "coral", color = "darkred") +
      coord_flip() +
      labs(
        title = paste("Model Comparison by", metric_col),
        x = "Model",
        y = metric_col
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 4: PREDICTION DIAGNOSTICS - Actual vs Predicted
  # ───────────────────────────────────────────────────────────────────────────
  
  output$actual_vs_predicted_plot <- renderPlot({
    if (!predictions_available) return(NULL)
    
    predictions_df %>%
      ggplot(aes(x = actual, y = predicted)) +
      geom_point(alpha = 0.5, color = "steelblue", size = 1.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Actual vs Predicted Values",
        x = "Actual GHLTH_CrudePrev",
        y = "Predicted GHLTH_CrudePrev"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        aspect.ratio = 1
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 4: PREDICTION DIAGNOSTICS - Residuals Histogram
  # ───────────────────────────────────────────────────────────────────────────
  
  output$residuals_histogram <- renderPlot({
    if (!predictions_available) return(NULL)
    
    predictions_df %>%
      ggplot(aes(x = residuals)) +
      geom_histogram(fill = "lightgreen", color = "darkgreen", bins = 30) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = "Residuals Distribution",
        x = "Residuals (Actual - Predicted)",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold")
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 4: PREDICTION DIAGNOSTICS - Summary Table
  # ───────────────────────────────────────────────────────────────────────────
  
  output$prediction_summary_table <- DT::renderDataTable({
    if (!predictions_available) return(NULL)
    
    summary_stats <- data.frame(
      Metric = c("RMSE", "MAE", "Mean Residual", "SD Residuals"),
      Value = c(
        sqrt(mean(predictions_df$residuals^2)),
        mean(abs(predictions_df$residuals)),
        mean(predictions_df$residuals),
        sd(predictions_df$residuals)
      )
    ) %>%
      mutate(Value = round(Value, 4))
    
    DT::datatable(
      summary_stats,
      options = list(pageLength = 4),
      rownames = FALSE
    )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 5: FEATURE IMPORTANCE - Random Forest Importance Plot
  # ───────────────────────────────────────────────────────────────────────────
  
  output$importance_plot <- renderPlot({
    if (!importance_available) return(NULL)
    
    rf_importance %>%
      top_n(10, Importance) %>%
      arrange(Importance) %>%
      ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col(fill = "purple", color = "darkviolet") +
      coord_flip() +
      labs(
        title = "Random Forest: Top 10 Important Features",
        x = "Feature",
        y = "% Increase in MSE"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold")
      )
  })
  
  # ───────────────────────────────────────────────────────────────────────────
  # TAB 5: FEATURE IMPORTANCE - Lasso Selected Features
  # ───────────────────────────────────────────────────────────────────────────
  
  output$lasso_features_table <- DT::renderDataTable({
    if (!lasso_available) return(NULL)
    
    features_display <- data.frame(
      Feature = lasso_features
    )
    
    DT::datatable(
      features_display,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)

