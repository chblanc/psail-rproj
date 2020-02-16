library(readr)
library(rsample)

library(future)

library(mlr3)
library(mlr3measures)
library(mlr3tuning)
library(mlr3pipelines)
library(mlr3learners)
library(mlr3filters)

# load-data -----------------------------------------------------------------

data <- readr::read_csv('data/credit_default.csv')
data <- mutate_if(data, is.character, as.factor)
data <- mutate(data, default_next_month = as.factor(ifelse(default_next_month==0, 'Good', 'Bad')))

set.seed(1337)
data <- rsample::initial_split(data, prop=0.8, strata='default_next_month')
train_df <- training(data)
test_df <- testing(data)

# define-a-pipeline ---------------------------------------------------------

target <- 'default_next_month'
xnames <- names(train_df)[!grepl('id|default', names(train_df))]
task <- mlr3::TaskClassif$new(id='credit-default', backend=train_df, target=target)

measure <- mlr3pipelines::mlr_measures$get('classif.fbeta')
dummy_encoder <- mlr3pipelines::mlr_pipeops$get("encode")
var_filter <- mlr3pipelines::po("filter", filter=mlr3filters::FilterVariance$new())

col_selector <- po("select")
col_selector$param_set$values$selector <- mlr3pipelines::selector_name(xnames)

log_it <- function(x) log1p(ifelse(x <= 0, 0, x))
col_logger <- po('colapply', applicator=log_it, affect_columns=selector_grep('balance|amt'))

imputer <- po('imputemedian', affect_columns=selector_name('education'))
balancer <- mlr3pipelines::po('classbalancing')
scaler <- mlr3pipelines::po('scalerange')

learner <- mlr_learners$get('classif.xgboost')

# graph ---------------------------------------------------------------------

graph <- col_selector %>>%
  balancer %>>% 
  dummy_encoder %>>%
  col_logger %>>%
  imputer %>>%
  scaler %>>%
  learner

glrn <- GraphLearner$new(graph)

# param set -----------------------------------------------------------------

ps <- paradox::ParamSet$new(list(
  paradox::ParamDbl$new("classbalancing.ratio", lower=0.5, upper=1),
  paradox::ParamFct$new("classbalancing.adjust", c('major')),
  paradox::ParamFct$new("classbalancing.reference", c('minor')),
  paradox::ParamInt$new('classif.xgboost.nrounds', lower=10, upper=100),
  paradox::ParamDbl$new('classif.xgboost.eta', lower=0.1, upper=0.75),
  paradox::ParamDbl$new('classif.xgboost.gamma', lower=1, upper=10),
  paradox::ParamInt$new('classif.xgboost.max_depth', lower=2, upper=20),
  paradox::ParamDbl$new('classif.xgboost.subsample', lower=0.3, upper=0.75),
  paradox::ParamDbl$new('classif.xgboost.colsample_bytree', lower=0.1, upper=0.9),
  paradox::ParamFct$new('classif.xgboost.booster', c('gbtree', 'dart'))
))

# tuning --------------------------------------------------------------------
future::plan("multiprocess")

instance <- mlr3tuning::TuningInstance$new(
  task = task,
  learner = glrn,
  resampling = mlr3::rsmp("repeated_cv", folds=10, repeats=1),
  measures = mlr3::msr("classif.fbeta"),
  param_set = ps,
  terminator = mlr3tuning::term("evals", n_evals=50)
)

tuner <- mlr3tuning::TunerRandomSearch$new()
tuner$tune(instance)
instance$result

# out-of-sample -------------------------------------------------------------

glrn$param_set$values <- instance$result$params
glrn$train(task)
glrn$predict_newdata(task, newdata=test_df)$score(measure)
