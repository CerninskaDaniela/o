# ══════════════════════════════════════════════════════════════════════════════
# CAPSTONE PROJECT SHINY APPLICATION
# MVP for GHLTH_CrudePrev Prediction Model Exploration
# ══════════════════════════════════════════════════════════════════════════════
#
# This Shiny app provides an interactive interface for exploring:
# - The PLACES dataset
# - Target variable (GHLTH_CrudePrev: fair/poor general health prevalence)
# - Selected behavioral and socioeconomic predictors
# - Model comparison and interpretation
#
# RECOMMENDED: Export these .rds files from final_project_pipeline.R for full functionality:
#   - model_results.rds (data frame with RMSE, MAE, R² for all models)
#   - predictions_df.rds (data frame with actual, predicted, residuals)
#   - rf_importance.rds (Random Forest variable importance)
#   - lasso_selected_features.rds (Lasso selected feature names)
#
# If .rds files are missing, the app loads and preprocesses the CSV directly,
# showing placeholders for advanced features (predictions, importance).
#
# ══════════════════════════════════════════════════════════════════════════════

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)

# ══════════════════════════════════════════════════════════════════════════════
# DATA LOADING AND PREPROCESSING
# ══════════════════════════════════════════════════════════════════════════════

# Define target and predictors (consistent with final_project_pipeline.R)
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


# ══════════════════════════════════════════════════════════════════════════════
# SHINY USER INTERFACE
# ══════════════════════════════════════════════════════════════════════════════

ui <- navbarPage(
  title = "GHLTH Prediction: Behavioral & Socioeconomic Determinants",
  
  # Theme and styling
  theme = bslib::bs_theme(version = 5, preset = "flatly"),
  
  # ═════════════════════════════════════════════════════════════════════════════
  # TAB 1: OVERVIEW
  # ═════════════════════════════════════════════════════════════════════════════
  
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
                tags$li(strong("ACCESS2_CrudePrev"), ": Limited access to places for physical activity"),
                tags$li(strong("BINGE_CrudePrev"), ": Binge drinking prevalence"),
                tags$li(strong("CSMOKING_CrudePrev"), ": Current smoking prevalence")
              )
            ),
            column(
              6,
              h5("Health Behaviors & Living Conditions"),
              tags$ul(
                tags$li(strong("LPA_CrudePrev"), ": Lack of leisure-time physical activity"),
                tags$li(strong("SLEEP_CrudePrev"), ": Insufficient sleep"),
                tags$li(strong("OBESITY_CrudePrev"), ": Obesity prevalence")
              )
            )
          ),
          fluidRow(
            column(
              6,
              h5("Socioeconomic Determinants"),
              tags$ul(
                tags$li(strong("FOODINSECU_CrudePrev"), ": Food insecurity"),
                tags$li(strong("HOUSINSECU_CrudePrev"), ": Housing insecurity")
              )
            ),
            column(
              6,
              h5("Utility Access"),
              tags$ul(
                tags$li(strong("SHUTUTILITY_CrudePrev"), ": Shut-off utility burden"),
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
  
  # ═════════════════════════════════════════════════════════════════════════════
  # TAB 2: DATA EXPLORER
  # ═════════════════════════════════════════════════════════════════════════════
  
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
            p(strong("Features (Predictors):"), length(PREDICTORS)),
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
              choices = c(TARGET, PREDICTORS),
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
              choices = PREDICTORS,
              selected = PREDICTORS[1]
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
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# SHINY SERVER LOGIC
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
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


# ══════════════════════════════════════════════════════════════════════════════
# RUN SHINY APPLICATION
# ══════════════════════════════════════════════════════════════════════════════

shinyApp(ui = ui, server = server)
