##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param preproc_train
##' @param preproc_test
##' @param xgb_params
fit_xgb <- function(preproc_train, data_folds, preproc_test, xgb_params) {

  .trn_X <- as.matrix(select(preproc_train, -outcome))
  .trn_Y <- as.numeric(pull(preproc_train, outcome) == 'yes')
  
  .tst_X <- as.matrix(select(preproc_test, -outcome))
  .tst_Y <- as.numeric(pull(preproc_test, outcome) == 'yes')
  
  set.seed(32989)
  
  xgb_fit_autotuner <- xgb.cv(
    params  = xgb_params,
    folds   = map(data_folds$splits, complement),
    data    = .trn_X,
    label   = .trn_Y,
    nrounds = 10000,
    early_stopping_rounds = 50,
    metrics = 'auc',
  )
  
  xgb_fit_final <- xgboost(
    params = xgb_params,
    data = .trn_X,
    label = .trn_Y,
    nrounds = xgb_fit_autotuner$best_iteration
  )
  
  xgb_prd_probs <- predict(xgb_fit_final, newdata = .tst_X)
  
  auc <- roc_auc_vec(
    truth    = factor(.tst_Y), 
    estimate = xgb_prd_probs
  )
  
  importance <- xgb.importance(model = xgb_fit_final)
  
  list(
    fit = xgb_fit_final,
    auc = auc,
    imp = importance
  )

}
