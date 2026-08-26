# ============================================================
# DECISION TREE MODEL — BASKET VALUE
# ============================================================

# Load packages
library(tidyverse)
library(rpart)
library(rpart.plot)

# ============================================================
# 1. LOAD TRAINING AND TEST DATA
# ============================================================

train <- read.csv("data/processed/train_cleaned.csv")
test <- read.csv("data/processed/test_cleaned.csv")

# Check dimensions
dim(train)
dim(test)

# Check variable names
names(train)

# Check structure
str(train)

# ============================================================
# 2. PREPARE VARIABLES FOR DECISION TREE
# ============================================================

# Convert categorical variables to factors
categorical_vars <- c(
  "customer_segment",
  "service_type",
  "city",
  "payment_method",
  "booking_channel",
  "time_period",
  "is_weekend",
  "promo_code_used",
  "traffic_level",
  "weather_condition"
)

train[categorical_vars] <- lapply(train[categorical_vars], factor)
test[categorical_vars] <- lapply(test[categorical_vars], factor)

str(train)

# Check missing values
colSums(is.na(train))
colSums(is.na(test))


# ============================================================
# 3. Baseline DECISION TREE
# ============================================================
decision_tree <- rpart(
  basket_value_vnd ~ customer_segment +
    service_type +
    customer_age +
    city +
    payment_method +
    booking_channel +
    distance_km_clean +
    estimated_duration_min +
    time_period +
    is_weekend +
    promo_code_used +
    discount_amount_vnd +
    traffic_level +
    weather_condition,
  data = train,
  method = "anova"
)

print(decision_tree)
summary(decision_tree)

# ============================================================
# 4. BASELINE TREE - TEST SET PREDICTIONS
# ============================================================

tree_pred <- predict(decision_tree, newdata = test)

# RMSE
tree_rmse <- sqrt(mean((test$basket_value_vnd - tree_pred)^2))

# MAE
tree_mae <- mean(abs(test$basket_value_vnd - tree_pred))

# R-squared
tree_r2 <- 1 -
  sum((test$basket_value_vnd - tree_pred)^2) /
  sum((test$basket_value_vnd - mean(test$basket_value_vnd))^2)

tree_rmse
tree_mae
tree_r2

# ============================================================
# 5. PRUNE DECISION TREE
# ============================================================

# Find CP corresponding to minimum cross-validated error
best_cp <- decision_tree$cptable[
  which.min(decision_tree$cptable[, "xerror"]),
  "CP"
]

best_cp

pruned_tree <- prune(
  decision_tree,
  cp = best_cp
)

print(pruned_tree)

rpart.plot(
  pruned_tree,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE
)

# ============================================================
# 6. PRUNED TREE - TEST PERFORMANCE
# ============================================================

pruned_pred <- predict(pruned_tree, newdata = test)

pruned_rmse <- sqrt(
  mean((test$basket_value_vnd - pruned_pred)^2)
)

pruned_mae <- mean(
  abs(test$basket_value_vnd - pruned_pred)
)

pruned_r2 <- 1 -
  sum((test$basket_value_vnd - pruned_pred)^2) /
  sum((test$basket_value_vnd - mean(test$basket_value_vnd))^2)

pruned_rmse
pruned_mae
pruned_r2

# ============================================================
# 7. COMPARE TREE COMPLEXITIES
# ============================================================

# Create trees with different CP values
tree_4 <- prune(
  decision_tree,
  cp = decision_tree$cptable[2, "CP"]
)

tree_11 <- prune(
  decision_tree,
  cp = decision_tree$cptable[3, "CP"]
)

# Predictions
pred_4 <- predict(tree_4, newdata = test)
pred_11 <- predict(tree_11, newdata = test)

# Performance function
tree_metrics <- function(actual, predicted) {
  
  rmse <- sqrt(mean((actual - predicted)^2))
  
  mae <- mean(abs(actual - predicted))
  
  r2 <- 1 - sum((actual - predicted)^2) /
    sum((actual - mean(actual))^2)
  
  return(c(
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  ))
}

# Calculate metrics
metrics_0 <- tree_metrics(test$basket_value_vnd, pruned_pred)
metrics_4 <- tree_metrics(test$basket_value_vnd, pred_4)
metrics_11 <- tree_metrics(test$basket_value_vnd, pred_11)
metrics_12 <- tree_metrics(test$basket_value_vnd, tree_pred)

# Combine results
tree_comparison <- rbind(
  "0 splits" = metrics_0,
  "4 splits" = metrics_4,
  "11 splits" = metrics_11,
  "12 splits" = metrics_12
)

tree_comparison




# ============================================================
# 8. BASELINE RANDOM FOREST
# ============================================================

library(randomForest)
# Check factor levels in train and test
lapply(train[categorical_vars], levels)
lapply(test[categorical_vars], levels)

set.seed(123)

random_forest <- randomForest(
  basket_value_vnd ~
    customer_segment +
    service_type +
    customer_age +
    city +
    payment_method +
    booking_channel +
    distance_km_clean +
    estimated_duration_min +
    time_period +
    is_weekend +
    promo_code_used +
    discount_amount_vnd +
    traffic_level +
    weather_condition,
  data = train,
  ntree = 500,
  importance = TRUE
)

print(random_forest)
importance(random_forest)
varImpPlot(random_forest)

# ============================================================
# 9. RANDOM FOREST TEST PERFORMANCE
# ============================================================

rf_pred <- predict(random_forest, newdata = test)

# RMSE
rf_rmse <- sqrt(
  mean((test$basket_value_vnd - rf_pred)^2)
)

# MAE
rf_mae <- mean(
  abs(test$basket_value_vnd - rf_pred)
)

# R-squared
rf_r2 <- 1 -
  sum((test$basket_value_vnd - rf_pred)^2) /
  sum((test$basket_value_vnd - mean(test$basket_value_vnd))^2)

rf_rmse
rf_mae
rf_r2

library(tidyverse)

# ============================================================
# RELATIVE TEST ERROR (% DIFFERENCE FROM MLR1)
# ============================================================

library(tidyverse)

# Model results
model_results <- tibble(
  Model = c(
    "MLR1",
    "Pruned Decision Tree",
    "Random Forest"
  ),
  RMSE = c(115015, 114261, 115718),
  MAE = c(100157, 99342, 100558)
)

# Calculate percentage difference relative to MLR1
relative_error <- model_results %>%
  mutate(
    RMSE_pct = (RMSE - RMSE[Model == "MLR1"]) /
      RMSE[Model == "MLR1"] * 100,
    
    MAE_pct = (MAE - MAE[Model == "MLR1"]) /
      MAE[Model == "MLR1"] * 100
  ) %>%
  select(Model, RMSE_pct, MAE_pct) %>%
  pivot_longer(
    cols = c(RMSE_pct, MAE_pct),
    names_to = "Metric",
    values_to = "Difference"
  ) %>%
  mutate(
    Metric = case_when(
      Metric == "RMSE_pct" ~ "RMSE",
      Metric == "MAE_pct" ~ "MAE"
    )
  )

# Check values
print(relative_error)


# ============================================================
# PLOT
# ============================================================

relative_error_plot <- ggplot(
  relative_error,
  aes(
    x = Model,
    y = Difference,
    group = Metric,
    linetype = Metric,
    color = Metric
  )
) +
  
  # MLR1 benchmark
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  
  # Lines
  geom_line(
    linewidth = 1
  ) +
  
  # Points
  geom_point(
    size = 3
  ) +
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

geom_text(
  data = relative_error %>%
    filter(Metric == "RMSE"),
  aes(
    label = sprintf("%.2f%%", Difference)
  ),
  nudge_y = 0.08,
  size = 3.5,
  show.legend = FALSE
) +
  
  geom_text(
    data = relative_error %>%
      filter(Metric == "MAE"),
    aes(
      label = sprintf("%.2f%%", Difference)
    ),
    nudge_y = -0.08,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  # ----------------------------------------------------------
# Colours matching your existing visual
# ----------------------------------------------------------

scale_color_manual(
  values = c(
    "RMSE" = "#2A9D8F",
    "MAE" = "#666666"
  )
) +
  
  # ----------------------------------------------------------
# Axis
# ----------------------------------------------------------

scale_y_continuous(
  limits = c(-1.0, 0.8),
  breaks = seq(-1.0, 0.8, 0.2),
  labels = function(x) paste0(x, "%")
) +
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

labs(
  title = "Relative Out-of-Sample Prediction Error",
  subtitle = "Percentage difference in test error relative to MLR1",
  x = NULL,
  y = "Difference from MLR1 (%)",
  color = NULL,
  linetype = NULL
) +
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "top",
    
    axis.text.x = element_text(
      size = 11
    ),
    
    axis.text.y = element_text(
      size = 10
    )
  )


# Display
print(relative_error_plot)


# ============================================================
# SAVE
# ============================================================

ggsave(
  "relative_test_error_percentage.png",
  plot = relative_error_plot,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# DECISION TREE RESULTS
# ============================================================

# Baseline Decision Tree:
# - 12 splits
# - Test RMSE = 122,077.6 VND
# - Test MAE = 105,142.9 VND
# - Test R2 = -0.142

# Pruned Decision Tree:
# - Cross-validation selected 0 splits
# - Test RMSE = 114,261.1 VND
# - Test MAE = 99,342.4 VND
# - Test R2 = -0.0001
# - Best-performing tree based on test-set metrics

# Tree variable importance:
# - Distance = 32%
# - Estimated duration = 20%
# - City = 20%
# - Weather condition = 7%
# - Promo code used = 6%
# - Time period = 5%

# Overall Decision Tree conclusion:
# - Increasing tree complexity reduced test-set performance.
# - The selected pruned tree had 0 splits and essentially predicted
#   the average Basket Value for all observations.
# - The Decision Tree did not identify stable predictive splits
#   that improved generalisation to unseen data.


# ============================================================
# RANDOM FOREST RESULTS
# ============================================================

# Baseline Random Forest:
# - 500 trees
# - 4 variables tried at each split
# - Test RMSE = 115,717.8 VND
# - Test MAE = 100,558.4 VND
# - Test R2 = -0.026

# Random Forest variable importance (%IncMSE):
# - Estimated duration = 6.08%
# - Distance = 4.29%
# - Payment method = 3.84%
# - Service type = 2.14%
# - Discount amount = 1.73%
# - Weather condition = 1.72%
# - Promo code used = 1.45%

# Random Forest model output:
# - Out-of-bag % variance explained = -2.71%

# Overall Random Forest conclusion:
# - Random Forest performed slightly worse than the pruned
#   Decision Tree on the test set.
# - The ensemble model did not provide a meaningful improvement
#   in predictive performance.
# - Estimated duration and distance were the most important
#   predictors based on %IncMSE.


# ============================================================
# OVERALL TREE-BASED MODEL CONCLUSION
# ============================================================

# - The pruned Decision Tree was the better-performing tree-based
#   model, with lower RMSE and MAE than Random Forest.
# - Both models had approximately zero or negative test-set R2.
# - This indicates weak generalisation performance for both
#   tree-based approaches.
# - The results suggest that the available predictors provide
#   limited predictive information for Basket Value using
#   tree-based modelling.
# - The next step is to compare these results with the
#   five MLR models and identify the best overall model.