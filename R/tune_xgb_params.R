##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_folds
##' @param recipe
tune_xgb_params <- function(data_folds = data_folds, recipe = recipe) {
  
  data_folds_prepped <- data_folds %>% 
    mutate(
      trn = map(splits, training), 
      tst = map(splits, testing),
      .prep = map(trn, ~prep(recipe, training = .x)),
      .trn = map(.prep, juice),
      .tst = map2(.prep, tst, bake)
    )
  
}
