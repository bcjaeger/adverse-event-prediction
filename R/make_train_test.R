##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_analysis
##' @param train_years
make_train_test <- function(data_analysis, train_years) {

  time_cutpoint <- max(train_years)
  
  prop_train <- mean(data_analysis$m0_impl_yr < time_cutpoint)
  
  rsample::initial_time_split(data_analysis, prop = prop_train)

}
