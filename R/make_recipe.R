##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_train
make_recipe <- function(data_train, outcome_colname) {
  
  dummy_namer <- function(var, lvl, ordinal = FALSE, sep = '..'){
    dummy_names(var = var, lvl = lvl, ordinal = ordinal, sep = sep)
  }
  
  fctrs_gt4levels <- data_train %>% 
    select_if(is.factor) %>% 
    summarize(across(everything(),~length(levels(.x)))) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value > 4) %>% 
    pull(name)

  recipe(x = data_train, formula = outcome~.) %>% 
    step_rm(patient_id, age2) %>% 
    step_other(one_of(fctrs_gt4levels), threshold = 0.05) %>% 
    step_nzv(all_predictors()) %>% 
    step_modeimpute(matches('_cc_|_cc2_')) %>% 
    # step_discretize_cart(
    #   all_numeric(), 
    #   outcome = vars('outcome'), 
    #   tree_depth = 3,
    #   cost_complexity = 0.001
    # ) %>%
    step_dummy(all_nominal(), -all_outcomes(), naming = dummy_namer) %>% 
    step_pca(matches('_cc_|_cc2_'), num_comp = 3)

}




