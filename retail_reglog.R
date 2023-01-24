library(tidyverse)

retail <- read_csv("data/retail/response_retail_v2.csv")

retail_transform <- retail |> 
  mutate(
    across(.cols = c(gender, marital_status, payment_channel),
           .fns = factor)
  ) |> 
  mutate(
    response = factor(response, levels = c(0, 1), 
                      labels = c("No", "Yes"))
  )

retail_transform |> 
  glimpse()


# Machine Learning ---------------------------------------------------


library(tidymodels)

# Split Data ---------------------------------------------------------

set.seed(1001)
retail_split <- retail_transform |> 
  initial_split(prop = 0.7, strata = response)

retail_split

retail_training <- retail_split |> 
  training() |> 
  select(-member_id)

retail_testing <- retail_split |> 
  testing()

# cv_folds <- vfold_cv(retail_training, 
#                      v = 5, repeats = 3, 
#                      strata = response)

retail_training |> 
  count(response) |> 
  mutate(pct = n/sum(n))

retail_testing |> 
  count(response) |> 
  mutate(pct = n/sum(n))

########################## Training Model ##########################


# Data Preprocess with {recipes} -------------------------------------

retail_recipe <- recipe(response ~ ., data = retail_training) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_factor_predictors()) |> 
  step_corr(all_numeric_predictors()) |> 
  prep(retain = TRUE)

retail_recipe

retail_recipe |> 
  tidy()

retail_recipe |> 
  juice()


# Logistics Regression ----------------------------------------------

# Model Specification
reglog_spec <- logistic_reg() |> 
  set_engine("glm") |> 
  set_mode("classification")

# Workflow
reglog_workflow <- 
  workflow() |> 
  add_recipe(retail_recipe) |> 
  add_model(reglog_spec)

# Model fitting
reglog_model <- reglog_workflow |> 
  fit(data = retail_training)

reglog_model |> 
  tidy() |> 
  print(n = Inf)


# Performance -------------------------------------------------------

predict(reglog_model, new_data = retail_testing)
predict(reglog_model, new_data = retail_testing, type = "prob")

predict(reglog_model, new_data = retail_testing, type = "prob") |> 
  select(.pred_Yes)

# Bind testing data with prediction results
reglog_pred <- retail_testing |> 
  select(response) |> 
  bind_cols(
    predict(reglog_model, new_data = retail_testing),
    predict(reglog_model, new_data = retail_testing, type = "prob") |> 
      select(-.pred_No)
  )

# Performance Metrics
reglog_pred |> 
  count(response)

reglog_pred |> 
  count(.pred_class)

# Confusion Matrix
reglog_pred |> 
  conf_mat(truth = response, estimate = .pred_class)

reglog_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot()

reglog_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

reglog_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  tidy() |> 
  mutate(pct = value/nrow(reglog_pred))

# Accuracy
reglog_pred |> 
  accuracy(truth = response, estimate = .pred_class)

# Sensitivity/Recall
reglog_pred |> 
  sensitivity(truth = response, estimate = .pred_class)

reglog_pred |> 
  recall(truth = response, estimate = .pred_class)

# Specificity
reglog_pred |> 
  specificity(truth = response, estimate = .pred_class)

# Precision
reglog_pred |> 
  precision(truth = response, estimate = .pred_class)

# ROC & AUC
reglog_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")

reglog_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot()

# F1 Score
reglog_pred |> 
  f_meas(truth = response, estimate = .pred_class)

reglog_summarise_metrics <- bind_rows(
  reglog_pred |> 
    accuracy(truth = response, estimate = .pred_class),
  reglog_pred |> 
    sensitivity(truth = response, estimate = .pred_class),
  reglog_pred |> 
    specificity(truth = response, estimate = .pred_class), 
  reglog_pred |> 
    recall(truth = response, estimate = .pred_class), 
  reglog_pred |> 
    precision(truth = response, estimate = .pred_class),
  reglog_pred |> 
    bal_accuracy(truth = response, estimate = .pred_class), 
  reglog_pred |> 
    roc_auc(truth = response, estimate = .pred_Yes, 
            event_level = "second"), 
  reglog_pred |> 
    f_meas(truth = response, estimate = .pred_class)
)

reglog_summarise_metrics
