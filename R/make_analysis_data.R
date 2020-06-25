##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pre
##' @param post
##' @param outcome_colname
make_analysis_data <- function(pre, post, outcome_colname, train_years) {

  outcome_data <- post$data %>% 
    select(patient_id, all_of(outcome_colname)) %>% 
    rename(outcome := !!outcome_colname) %>% 
    mutate(
      outcome = if_else(outcome > 0, 'yes', 'no'),
      outcome = factor(outcome, levels = c('no', 'yes'))
    ) %>% 
    drop_na()
  
  outcome_data %>% 
    left_join(pre$data) %>% 
    select(-starts_with('pt_outcome'), -oper_id) %>% 
    filter(m0_impl_yr > min(train_years)) %>% 
    mutate(
      across(where(is.factor), fct_explicit_na, na_level = 'missing'),
      m0_primary_dgn = factor(m0_primary_dgn)
    )

}
