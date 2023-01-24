library(tidyverse)

retail <- read_csv("data/response_retail_v2.csv")

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

# Bind training data with prediction results
reglog_train <- retail_training |> 
  select(response) |> 
  bind_cols(
    predict(reglog_model, new_data = retail_training),
    predict(reglog_model, new_data = retail_training, type = "prob") |> 
      select(-.pred_No)
  )

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
reglog_train |> 
  conf_mat(truth = response, estimate = .pred_class)

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
reglog_train |> 
  accuracy(truth = response, estimate = .pred_class)

reglog_pred |> 
  accuracy(truth = response, estimate = .pred_class)

# Sensitivity/Recall
reglog_train |> 
  sensitivity(truth = response, estimate = .pred_class)

reglog_pred |> 
  sensitivity(truth = response, estimate = .pred_class)

reglog_pred |> 
  recall(truth = response, estimate = .pred_class)

# Specificity
reglog_train |> 
  specificity(truth = response, estimate = .pred_class)

reglog_pred |> 
  specificity(truth = response, estimate = .pred_class)

# Precision
reglog_train |> 
  precision(truth = response, estimate = .pred_class)

reglog_pred |> 
  precision(truth = response, estimate = .pred_class)

# ROC & AUC
reglog_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot()

reglog_train |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")
reglog_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")

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

# Dectree -----------------------------------------------------------

dectree_spec <- decision_tree() |> 
  set_engine("rpart", model = TRUE) |> 
  set_mode("classification")

dectree_workflow <- 
  workflow() |> 
  add_recipe(retail_recipe) |> 
  add_model(dectree_spec)

dectree_model <- dectree_workflow |> 
  fit(data = retail_training)

library(rpart.plot)
dectree_model |> 
  extract_fit_engine() |> 
  rpart.plot(type = 5, extra = 104, tweak = 1.2)


# Performance -------------------------------------------------------

predict(dectree_model, new_data = retail_testing)
predict(dectree_model, new_data = retail_testing, type = "prob")

predict(dectree_model, new_data = retail_testing, type = "prob") |> 
  select(.pred_Yes)

dectree_pred <- retail_testing |> 
  select(response) |> 
  bind_cols(
    predict(dectree_model, new_data = retail_testing),
    predict(dectree_model, new_data = retail_testing, type = "prob") |> 
      select(.pred_Yes)
  )

dectree_pred |> 
  count(response)

dectree_pred |> 
  count(.pred_class)

dectree_pred |> 
  conf_mat(truth = response, estimate = .pred_class) 

dectree_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot()

dectree_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

dectree_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  tidy() |> 
  mutate(pct = value/nrow(dectree_pred))

dectree_pred |> 
  accuracy(truth = response, estimate = .pred_class)

dectree_pred |> 
  sensitivity(truth = response, estimate = .pred_class)

dectree_pred |> 
  specificity(truth = response, estimate = .pred_class)

dectree_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot()

dectree_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")

dectree_pred |> 
  recall(truth = response, estimate = .pred_class)

dectree_pred |> 
  precision(truth = response, estimate = .pred_class)

dectree_pred |> 
  f_meas(truth = response, estimate = .pred_class)

dectree_summarise_metrics <- 
  bind_rows(
    dectree_pred |> 
      accuracy(truth = response, estimate = .pred_class),
    dectree_pred |> 
      sensitivity(truth = response, estimate = .pred_class),
    dectree_pred |> 
      specificity(truth = response, estimate = .pred_class), 
    dectree_pred |> 
      roc_auc(truth = response, estimate = .pred_Yes, 
              event_level = "second"), 
    dectree_pred |> 
      recall(truth = response, estimate = .pred_class), 
    dectree_pred |> 
      precision(truth = response, estimate = .pred_class),
    dectree_pred |> 
      f_meas(truth = response, estimate = .pred_class)
  )

reglog_summarise_metrics
dectree_summarise_metrics

# Random Forest ------------------------------------------------------

rf_spec <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification")

rf_workflow <- 
  workflow() |> 
  add_recipe(retail_recipe) |> 
  add_model(rf_spec)

rf_model <- rf_workflow |> 
  fit(data = retail_training)

# Performance --------------------------------------------------------

predict(rf_model, new_data = retail_testing)
predict(rf_model, new_data = retail_testing, type = "prob")

predict(rf_model, new_data = retail_testing, type = "prob") |> 
  select(.pred_Yes)

rf_pred <- retail_testing |> 
  select(response) |> 
  bind_cols(
    predict(rf_model, new_data = retail_testing),
    predict(rf_model, new_data = retail_testing, type = "prob") |> 
      select(.pred_Yes)
  )

rf_pred |> 
  count(response)

rf_pred |> 
  count(.pred_class)

rf_pred |> 
  conf_mat(truth = response, estimate = .pred_class) 

rf_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot()

rf_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

rf_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  tidy() |> 
  mutate(pct = value/nrow(rf_pred))

rf_pred |> 
  accuracy(truth = response, estimate = .pred_class)

rf_pred |> 
  sensitivity(truth = response, estimate = .pred_class)

rf_pred |> 
  specificity(truth = response, estimate = .pred_class)

rf_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot()

rf_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")

rf_pred |> 
  recall(truth = response, estimate = .pred_class)

rf_pred |> 
  precision(truth = response, estimate = .pred_class)

rf_pred |> 
  f_meas(truth = response, estimate = .pred_class)

rf_summarise_metrics <- 
  bind_rows(
    rf_pred |> 
      accuracy(truth = response, estimate = .pred_class),
    rf_pred |> 
      sensitivity(truth = response, estimate = .pred_class),
    rf_pred |> 
      specificity(truth = response, estimate = .pred_class), 
    rf_pred |> 
      roc_auc(truth = response, estimate = .pred_Yes, 
              event_level = "second"), 
    rf_pred |> 
      recall(truth = response, estimate = .pred_class), 
    rf_pred |> 
      precision(truth = response, estimate = .pred_class),
    rf_pred |> 
      f_meas(truth = response, estimate = .pred_class)
  )

dectree_summarise_metrics
reglog_summarise_metrics
rf_summarise_metrics

# XGBoost ------------------------------------------------------------

xgb_spec <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("classification")

xgb_workflow <- 
  workflow() |> 
  add_recipe(retail_recipe) |> 
  add_model(xgb_spec)

xgb_model <- xgb_workflow |> 
  fit(data = retail_training)


# Performance --------------------------------------------------------

predict(xgb_model, new_data = retail_testing)
predict(xgb_model, new_data = retail_testing, type = "prob")

predict(xgb_model, new_data = retail_testing, type = "prob") |> 
  select(.pred_Yes)

xgb_pred <- retail_testing |> 
  select(response) |> 
  bind_cols(
    predict(xgb_model, new_data = retail_testing),
    predict(xgb_model, new_data = retail_testing, type = "prob") |> 
      select(.pred_Yes)
  )

xgb_pred |> 
  count(response)

xgb_pred |> 
  count(.pred_class)

xgb_pred |> 
  conf_mat(truth = response, estimate = .pred_class) 

xgb_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot()

xgb_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  autoplot(type = "heatmap")

xgb_pred |> 
  conf_mat(truth = response, estimate = .pred_class) |> 
  tidy() |> 
  mutate(pct = value/nrow(xgb_pred))

xgb_pred |> 
  accuracy(truth = response, estimate = .pred_class)

xgb_pred |> 
  sensitivity(truth = response, estimate = .pred_class)

xgb_pred |> 
  specificity(truth = response, estimate = .pred_class)

xgb_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot()

xgb_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second")

xgb_pred |> 
  recall(truth = response, estimate = .pred_class)

xgb_pred |> 
  precision(truth = response, estimate = .pred_class)

xgb_pred |> 
  f_meas(truth = response, estimate = .pred_class)

xgb_summarise_metrics <- 
  bind_rows(
    xgb_pred |> 
      accuracy(truth = response, estimate = .pred_class),
    xgb_pred |> 
      sensitivity(truth = response, estimate = .pred_class),
    xgb_pred |> 
      specificity(truth = response, estimate = .pred_class), 
    xgb_pred |> 
      roc_auc(truth = response, estimate = .pred_Yes, 
              event_level = "second"), 
    xgb_pred |> 
      recall(truth = response, estimate = .pred_class), 
    xgb_pred |> 
      precision(truth = response, estimate = .pred_class),
    xgb_pred |> 
      f_meas(truth = response, estimate = .pred_class)
  )

reglog_summarise_metrics
dectree_summarise_metrics
rf_summarise_metrics
xgb_summarise_metrics

# Compare ------------------------------------------------------------

compare_metrics <- reglog_summarise_metrics |> 
  transmute(.metric, 
            reglog = .estimate) |> 
  left_join(
    dectree_summarise_metrics |> 
      transmute(.metric, 
                dectree = .estimate), 
    by = ".metric"
    ) |> 
  left_join(
    rf_summarise_metrics |> 
      transmute(.metric, 
                rf = .estimate), 
    by = ".metric"
  ) |> 
  left_join(
    xgb_summarise_metrics |> 
      transmute(.metric, 
                xgb = .estimate), 
    by = ".metric"
  )
compare_metrics

compare_metrics |> 
  pivot_longer(cols = c(reglog, dectree, rf, xgb), 
               names_to = "Algorithm", values_to = "Value") |> 
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("reglog", "dectree", "rf", "xgb"))) |> 
  ggplot(aes(x = .metric, y = Value, fill = Algorithm)) + 
  geom_col(position = position_dodge()) + 
  coord_flip() + 
  theme_minimal()

reglog_auc <- reglog_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second") |> 
  pull(.estimate)

reglog <- reglog_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(reglog_auc, 4))) + 
  ggtitle("Logistic Regression")


dectree_auc <- dectree_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second") |> 
  pull(.estimate)

dtree <- dectree_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(dectree_auc, 4))) + 
  ggtitle("Decision Tree")


rf_auc <- rf_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second") |> 
  pull(.estimate)

rf <- rf_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(rf_auc, 4))) + 
  ggtitle("Random Forest")


xgb_auc <- xgb_pred |> 
  roc_auc(truth = response, estimate = .pred_Yes, 
          event_level = "second") |> 
  pull(.estimate)

xgb <- xgb_pred |> 
  roc_curve(truth = response, estimate = .pred_Yes, 
            event_level = "second") |> 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(xgb_auc, 4))) + 
  ggtitle("XGBoost")

library(gridExtra)
grid.arrange(reglog, dtree, rf, xgb, ncol = 2)

compare_roc <- bind_rows(
  reglog_pred |> 
    roc_curve(truth = response, estimate = .pred_Yes, 
              event_level = "second") |> 
    mutate(Algorithm = "Reglog"),
  dectree_pred |> 
    roc_curve(truth = response, estimate = .pred_Yes, 
              event_level = "second") |> 
    mutate(Algorithm = "DecTree"),
  rf_pred |> 
    roc_curve(truth = response, estimate = .pred_Yes, 
              event_level = "second") |> 
    mutate(Algorithm = "RF"),
  xgb_pred |> 
    roc_curve(truth = response, estimate = .pred_Yes, 
              event_level = "second") |> 
    mutate(Algorithm = "XGBoost")
)

compare_roc |> 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = Algorithm)) + 
  geom_line() + 
  coord_fixed() + 
  theme_light()


# Model Interpetation -----------------------------------------------

library(DALEXtra)

explainer <- 
  explain_tidymodels(
    model = rf_model, 
    data = retail_training, 
    y = retail_training$response == "Yes",
    verbose = TRUE, 
    label = "Retail Response Prediction"
    )

set.seed(1001)
x <- retail_testing |> 
  nrow() |> 
  sample(3)
new_obs <- retail_testing |>
  dplyr::slice(x) |> 
  column_to_rownames("member_id")

rf_model |> 
  predict(new_obs)

library(modelStudio)
modelStudio(
  explainer = explainer, 
  new_observation = new_obs, 
  new_observation_y = new_obs$response, 
  max_features = 15, 
  facet_dim = c(2, 3)
  )











