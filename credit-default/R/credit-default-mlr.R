library(readr)
library(rsample)
library(dplyr)

library(future)
library(future.apply)

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

measure <- mlr3::mlr_measures$get('classif.fbeta')
dummy_encoder <- mlr3pipelines::mlr_pipeops$get("encode")
var_filter <- mlr3pipelines::po("filter", filter=mlr3filters::flt('variance'))

col_selector <- mlr3pipelines::po("select")
col_selector$param_set$values$selector <- mlr3pipelines::selector_name(xnames)

log_it <- function(x) log1p(ifelse(x <= 0, 0, x))
log_cols <- mlr3pipelines::selector_grep('balance|amt')
col_logger <- mlr3pipelines::po('colapply', applicator=log_it, affect_columns=log_cols)

imputer <- mlr3pipelines::po('imputemedian')
balancer <- mlr3pipelines::po('classbalancing', param_vals=list(adjust='major', reference='minor'))
scaler <- mlr3pipelines::po('scalerange')
pca_cols <- mlr3pipelines::selector_grep('\\d+')
pca <- mlr3pipelines::po('pca', param_vals=list(center=F,  scale.=F), affect_columns=pca_cols)
rm_constants <- mlr3pipelines::po('removeconstants')

learner <- mlr_learners$get('classif.ranger')
#learner <- mlr_learners$get('classif.xgboost')

# graph ---------------------------------------------------------------------

graph <- col_selector %>>%
  balancer %>>% 
  dummy_encoder %>>%
  imputer %>>%
  rm_constants %>>%
  var_filter %>>%
  col_logger %>>%
  scaler %>>%
  pca %>>%
  learner

# Check pre-processing?
#graph$train(task)[[1]]$data()

glrn <- GraphLearner$new(graph)

# param set -----------------------------------------------------------------

ps <- paradox::ParamSet$new(list(
  paradox::ParamDbl$new("classbalancing.ratio", lower=0.95, upper=1),
  #paradox::ParamFct$new("classbalancing.reference", c('minor')),
  paradox::ParamDbl$new("variance.filter.frac", lower=0.90, upper=1),
  paradox::ParamDbl$new("removeconstants.ratio", lower=0, upper=0.01),
  paradox::ParamInt$new("pca.rank.", lower=5, upper=12),
  paradox::ParamFct$new('classif.ranger.splitrule', c('extratrees')),
  paradox::ParamInt$new('classif.ranger.mtry', lower=2, upper=8),
  paradox::ParamInt$new('classif.ranger.min.node.size', lower=1, upper=5),
  paradox::ParamInt$new('classif.ranger.num.trees', lower=50, upper=500)
  #paradox::ParamInt$new('classif.xgboost.nrounds', lower=10, upper=250),
  #paradox::ParamDbl$new('classif.xgboost.eta', lower=0.1, upper=0.75),
  #paradox::ParamDbl$new('classif.xgboost.gamma', lower=1, upper=10),
  #paradox::ParamInt$new('classif.xgboost.max_depth', lower=2, upper=20),
  #paradox::ParamDbl$new('classif.xgboost.subsample', lower=0.1, upper=0.75),
  #paradox::ParamDbl$new('classif.xgboost.colsample_bytree', lower=0.1, upper=1.0),
  #paradox::ParamFct$new('classif.xgboost.booster', c('gbtree', 'dart'))
))

# tuning --------------------------------------------------------------------
future::plan("multiprocess")

instance <- mlr3tuning::TuningInstance$new(
  task = task,
  learner = glrn,
  resampling = mlr3::rsmp("repeated_cv", folds=10, repeats=3),
  measures = mlr3::msr("classif.fbeta"),
  param_set = ps,
  terminator = mlr3tuning::term("evals", n_evals=25)
)

tuner <- mlr3tuning::TunerRandomSearch$new()
tuner$tune(instance)
instance$result

instance$archive() %>% 
  as_tibble() %>% 
  select(-resample_result, -params) %>% 
  mutate(tune_x = map(tune_x, as_tibble)) %>%
  unnest() %>% 
  gather(k, v, matches('variance|remove|ranger|classbal')) %>% 
  qplot(v, classif.fbeta, data= ., geom=c('line', 'point') ) + 
  facet_wrap(~k, scales='free_x')

# out-of-sample -------------------------------------------------------------

glrn$param_set$values <- instance$result$params
glrn$train(task)
glrn$predict_newdata(task, newdata=test_df)$score(measure)



