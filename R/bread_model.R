## Groceries model

# https://www.kaggle.com/competitions/favorita-grocery-sales-forecasting/overview

# https://www.tidymodels.org/learn/models/time-series/

library(tidyverse)
library(lubridate)
library(here)
library(tidymodels)
library(glmnet)
library(xgboost)

# doParallel::registerDoParallel(core = 4)


# Read in data ------------------------------------------------------------

items <- read_csv(here("data/items.csv")) %>% 
  filter(family == "BREAD/BAKERY")
stores <- read_csv(here("data/stores.csv"))
transactions <- read_csv(here("data/transactions.csv"))
oil <- read_csv(here("data/oil.csv"))
holidays_events <- read_csv(here("data/holidays_events.csv"))

# Create test/train split
groceries_split <- 
  read_csv(here("data/train.csv")) %>%  
  filter(item_nbr == 502331) %>% 
  # Join item information - family, class, perishable
  left_join(items, by = "item_nbr") %>% 
  # Join store information - city, state, type, cluster
  left_join(stores, by = "store_nbr") %>% 
  left_join(transactions, by = c("date", "store_nbr")) %>% 
  initial_split(prop = 0.9, strata = store_nbr)

train <- training(groceries_split)
test <- testing(groceries_split)
  
sales_cv <- rsample::vfold_cv(train, v = 5, strata = store_nbr)



# Model intro -------------------------------------------------------------



# First simple model ------------------------------------------------------

## Pre-processing & feature selection --------------------------------------

prep_juice <- function(x) juice(prep(x))

# First model is a simple mode looking using stores and temporal data to predict unit sales
sales_recipe_simple <- recipe(unit_sales ~ store_nbr + date + id,
                             data = train) %>% 
  update_role(id, new_role = "id") %>% 
  step_date(date, keep_original_cols = F) %>% 
  step_mutate(date_year = as.factor(date_year)) %>% 
  step_dummy(all_nominal_predictors()) 

sales_recipe_simple %>% 
  prep_juice()


## Model specification -----------------------------------------------------

# XGBoost workflow
xgb_wf <- workflow() %>% 
  add_model(boost_tree() %>% 
  set_mode("regression")) %>% 
  add_recipe(sales_recipe_simple)

# Random Forest workflow
rf_wf <- workflow() %>% 
  add_model(rand_forest(mode = "regression")) %>% 
  add_recipe(sales_recipe_simple)



## Load custom metrics -----------------------------------------------------

# Two metric functions - rmsle and nwrmsle
source(here("R/metric_sets.R"))


## Fit resamples - first model XGBoost -------------------------------------

grid_control <- control_grid(save_pred = TRUE, verbose = TRUE)

mset <- metric_set(rmsle)

xgb_resampled <- xgb_wf %>%
  fit_resamples(resamples = sales_cv,
                metrics = metric_set(rmsle),
                control = grid_control)


collect_metrics(xgb_resampled)

# rmse = 0.385


## Fit resamples - first model random forest -------------------------------

rf_resampled <- rf_wf %>%
  fit_resamples(resamples = sales_cv,
                metrics = metric_set(rmsle),
                control = grid_control)


collect_metrics(rf_resampled)

# rmse = 0.5


# Feature engineering - trial multiple recipes ----------------------------

sales_recipe_full <- recipe(unit_sales ~ store_nbr + date + type + cluster + 
                                state + transactions + id,
                              data = train) %>% 
  update_role(id, new_role = "id") %>% 
  step_date(date, keep_original_cols = F) %>% 
  step_mutate(date_year = as.factor(date_year)) %>% 
  step_impute_knn(transactions, neighbors = 3, impute_with = imp_vars(store_nbr, date_dow, date_year)) %>% 
  step_dummy(all_nominal_predictors()) 

sales_recipe_full %>% 
  prep_juice() %>% 
  filter(is.na(transactions)) %>% View

## Adding in pay_day flag

sales_recipe_pay_day <- recipe(unit_sales ~ store_nbr + date + type + cluster + 
                              state + transactions + id,
                            data = train) %>% 
  update_role(id, new_role = "id") %>% 
  step_mutate(pay_day = if_else(date == ceiling_date(date, "month") - days(1) | day(date) == 15,1,0) %>%
                factor()) %>% 
  step_date(date, keep_original_cols = F) %>% 
  step_mutate(date_year = as.factor(date_year)) %>% 
  step_impute_knn(transactions, neighbors = 3, impute_with = imp_vars(store_nbr, date_dow, date_year)) %>%
  step_dummy(all_nominal_predictors()) 


## Model specification -----------------------------------------------------



xgb_model <- boost_tree(mode = "regression")
rf_model <- rand_forest(mode = "regression")

model_trial1_wfs <- workflow_set(models = list(xgb = xgb_model, rf = rf_model),
                             preproc = list(simple = sales_recipe_simple,
                                            full = sales_recipe_full,
                                            pay_day = sales_recipe_pay_day),
                             cross = T)

## Time series resampling
# https://workflowsets.tidymodels.org/

model_trial_resamples <- workflow_map(object = model_trial1_wfs,
                                      fn = "fit_resamples",
                                      resamples = sales_cv,
                                      metrics = mset,
                                      verbose = TRUE,
                                      control = grid_control)

collect_metrics(model_trial_resamples)

autoplot(model_trial_resamples)

## Residuals

model_trial_resamples %>%
  head(2) %>% 
  select(result) %>% 
  unnest(result) %>% 
  unnest(.predictions) %>% 
  ggplot(aes(unit_sales, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted unit sales",
    color = NULL
  )

# Fit final model

final_fitted_xgb <- workflow(preprocessor = sales_recipe_full,spec = xgb_model) %>% 
  last_fit(groceries_split,metrics = mset)

final_fitted_xgb %>% 
  collect_metrics()

## Variable importance

library(vip)


workflow(preprocessor = sales_recipe_full,spec = xgb_model) %>% 
  fit(data = train) %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")


### RESULTS ### ----

# - XGB run significantly faster (~10 times)
# - Results were similar
# - Transactions have the highest importance of all the models but the data set doesn't project into the test set (for obvious reasons)

# Next step, try to model all the BREAD/BAKERY family items together
# See if hyper-parameter tuning improves results significantly and better differentiates the models.