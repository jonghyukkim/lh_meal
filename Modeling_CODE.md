---
title: "Modeling_CODE"
date: "2021-07-11"
output: 
  html_document:
    fig_width: 14
    fig_height: 10
    fig.align : 'center'
    toc: yes
    number_sections : yes
    code_folding: show
    keep_md: true
---

<hr>



<style type="text/css">
.main-container {
  max-width: 1200px;
  margin-left: auto;
  margin-right: auto;
}
</style>


# Load package

```r
rm(list=ls())

#load packages
library(knitr)
library(tidyverse)
library(magrittr)
library(tidymodels)
library(caret)

options(scipen=999) # turn-off scientific notation like 1e+48
options(tibble.width = Inf)
```

<hr>

<div style="margin-bottom:60px;">
</div>

# Import Data 

```r
train <- read_csv("train_new.csv")
test <- read_csv("test_new.csv")
submission <- read_csv("data/sample_submission.csv")
```

<hr>

<div style="margin-bottom:60px;">
</div>

# Modeling

## N_lunch


```r
# Initial Train-Test Split – rsample
# set.seed(2021)
# train_split <- initial_split(train, prop = 0.80, breaks = N_lunch)

# training_set <- train_split %>% training()
# testing_set <- train_split %>% testing()

train_recipe <- train %>% 
  recipe(N_lunch ~ .) %>%
  step_rm(Date, Breakfast, Lunch, Dinner, N_dinner) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_string2factor(all_nominal())

train_recipe_xg <- train %>% 
  recipe(N_lunch ~ .) %>%
  step_rm(Date, Breakfast, Lunch, Dinner, N_dinner) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_string2factor(all_nominal()) %>% 
  step_dummy(all_nominal()) 

set.seed(2021)
cv_folds <- train %>% vfold_cv(v = 5, breaks = N_lunch)


# Linear Regression
lm_model_set <- linear_reg(
  mode = "regression", 
) %>%
  set_engine("lm")

# lm_cv_results <- tune_grid(object = lm_model_set, 
#                            preprocessor = train_recipe, 
#                            resamples = cv_folds, 
#                            metrics = metric_set(mae))

# lm_cv_results %>% show_best() # 75.0 


# RandomForest
# install.packages("randomForest")
library(randomForest)
rf_model_set <- rand_forest(
  mode = "regression",
  trees = 500,
  mtry = tune()
) %>%  
  set_engine("randomForest")

# Hyperparameter tunning : grid search
rf_params <- parameters(mtry(c(3,7)))
rf_grid <- grid_regular(rf_params, levels = 5)

# rf_cv_results <- tune_grid(object = rf_model_set,
#                            preprocessor = train_recipe,
#                            resamples = cv_folds,
#                            grid = rf_grid,
#                            metrics = metric_set(mae))

# rf_cv_results %>% show_best() # mtry=5, CV mae=70.2


# Xgboost
library(xgboost)
# Bayesian Optimization mtry & tree_depth & min_n
xg_model_set <- boost_tree(
  mode = "regression", 
  trees = 500,
  tree_depth = tune(),
  mtry = tune(),
  min_n = tune(),
  learn_rate = 0.01
) %>%
  set_engine("xgboost")

# Hyperparameter tunning : Bayesian Optimization
xgboost_params <- parameters(tree_depth(c(3,15)), mtry(c(3,7)), min_n(c(5,15)))

# bayes_cv_results <-
#   tune_bayes(
#     object = xg_model_set,
#     preprocessor = train_recipe_xg,
#     resamples = cv_folds,
#     # To use non-default parameter ranges
#     param_info = xgboost_params,
#     initial = 4,
#     iter = 30,
#     # How to measure performance?
#     metrics = metric_set(mae),
#     control = control_bayes(no_improve = 20, verbose = FALSE))

# Find best hyperparameter
# bayes_cv_results %>% show_best() # mae=67.1
# bayes_cv_results %>% select_best("mae") # mtry=6, min_n=5, tree_depth=8


# Final lunch model
trainset_final <- train_recipe_xg %>% prep() %>% bake(train)
testset_final <- train_recipe_xg %>% prep() %>% bake(test)

lunch_model <- boost_tree(
  mode = "regression", 
  trees = 500,
  tree_depth = 8,
  mtry = 6,
  min_n = 5,
  learn_rate = 0.01
) %>%
  set_engine("xgboost") %>% 
  fit(N_lunch ~., data = trainset_final)

predicted_lunch <- lunch_model %>%
  predict.model_fit(testset_final) %>%
  mutate(.pred = round(.pred)) %>% unlist()

submission$`중식계` <- predicted_lunch

# Importance plot
library(vip) # importance variable plot
vip(lunch_model,
    aesthetics = list(fill = "olivedrab")) +
  labs(title = "Xgboost Model Importance - type Prediction")
```

![](Modeling_CODE_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

<hr>

<div style="margin-bottom:60px;">
</div>

## N_dinner


```r
# Initial Train-Test Split – rsample
# set.seed(2021)
# train_split <- initial_split(train, prop = 0.80, breaks = N_dinner)

# training_set <- train_split %>% training()
# testing_set <- train_split %>% testing()

train_recipe <- train %>% 
  recipe(N_dinner ~ .) %>%
  step_rm(Date, Breakfast, Lunch, Dinner, N_lunch) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_string2factor(all_nominal())

train_recipe_xg <- train %>% 
  recipe(N_dinner ~ .) %>%
  step_rm(Date, Breakfast, Lunch, Dinner, N_lunch) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_string2factor(all_nominal()) %>% 
  step_dummy(all_nominal()) 

set.seed(2021)
cv_folds <- train %>% vfold_cv(v = 5, breaks = N_dinner)


# Linear Regression
lm_model_set <- linear_reg(
  mode = "regression", 
) %>%
  set_engine("lm")

# lm_cv_results <- tune_grid(object = lm_model_set, 
#                            preprocessor = train_recipe, 
#                            resamples = cv_folds, 
#                            metrics = metric_set(mae))

# lm_cv_results %>% show_best() # 71.5


# RandomForest
# install.packages("randomForest")
library(randomForest)
rf_model_set <- rand_forest(
  mode = "regression",
  trees = 500,
  mtry = tune()
) %>%  
  set_engine("randomForest")

# Hyperparameter tunning : grid search
rf_params <- parameters(mtry(c(3,7)))
rf_grid <- grid_regular(rf_params, levels = 5)

# rf_cv_results <- tune_grid(object = rf_model_set,
#                            preprocessor = train_recipe,
#                            resamples = cv_folds,
#                            grid = rf_grid,
#                            metrics = metric_set(mae))

# rf_cv_results %>% show_best() # mtry=5, CV mae=66.0


# Xgboost
library(xgboost)
# Bayesian Optimization mtry & tree_depth & min_n
xg_model_set <- boost_tree(
  mode = "regression", 
  trees = 500,
  tree_depth = tune(),
  mtry = tune(),
  min_n = tune(),
  learn_rate = 0.01
) %>%
  set_engine("xgboost")

# Hyperparameter tunning : Bayesian Optimization
xgboost_params <- parameters(tree_depth(c(5,15)), mtry(c(3,7)), min_n(c(5,15)))

# bayes_cv_results <-
#   tune_bayes(
#     object = xg_model_set,
#     preprocessor = train_recipe_xg,
#     resamples = cv_folds,
#     # To use non-default parameter ranges
#     param_info = xgboost_params,
#     initial = 4,
#     iter = 30,
#     # How to measure performance?
#     metrics = metric_set(mae),
#     control = control_bayes(no_improve = 20, verbose = FALSE))

# Find best hyperparameter
# bayes_cv_results %>% show_best() # mae=67.2
# bayes_cv_results %>% select_best("mae") # mtry=7, min_n=11, tree_depth=5


# Final dinner model
trainset_final <- train_recipe %>% prep() %>% bake(train)
testset_final <- train_recipe %>% prep() %>% bake(test)

dinner_model <- rand_forest(
  mode = "regression",
  mtry = 5,
  trees = 500
) %>% set_engine("randomForest") %>%
  fit(N_dinner ~., data = trainset_final)

predicted_dinner <- dinner_model %>%
  predict.model_fit(testset_final) %>%
  mutate(.pred = round(.pred)) %>% unlist()

submission$`석식계` <- predicted_dinner

# Importance plot
library(vip) # importance variable plot
vip(dinner_model,
    aesthetics = list(fill = "olivedrab")) +
  labs(title = "RandomForest Model Importance - type Prediction")
```

![](Modeling_CODE_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
submission
```

```
## # A tibble: 50 x 3
##    일자       중식계 석식계
##    <date>      <dbl>  <dbl>
##  1 2021-01-27   1024    389
##  2 2021-01-28    974    401
##  3 2021-01-29    611    255
##  4 2021-02-01   1167    504
##  5 2021-02-02   1000    446
##  6 2021-02-03    968    410
##  7 2021-02-04    932    462
##  8 2021-02-05    615    308
##  9 2021-02-08   1202    616
## 10 2021-02-09   1012    497
## # ... with 40 more rows
```

