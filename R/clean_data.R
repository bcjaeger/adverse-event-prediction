##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param visit
##' @param train_years
clean_data <- function(visit, train_years = c(2012, 2013, 2014, 2015)) {

  filename <- glue("Visit_{visit}.csv")
  outname <- glue("Visit_{visit}_clean.rds")
  
  data <- read_csv(file = glue("../../data/{filename}"), 
    na = c('', ' ', "NA", "Missing", "Unknown"),
    trim_ws = TRUE,
    guess_max = 19433) %>% 
    clean_names() %>% 
    mutate_if(is.character, clean_chr)
  
  cols <- tibble(
    description = 'all variables',
    count = ncol(data),
    dropped = "None"
  )
  
  data_trn <- filter(data, m0_impl_yr %in% train_years)
  
  cnst <- whichAreConstant(data_trn, verbose = TRUE)
  
  vars_dropped <- names(data_trn)[cnst]
  
  if(!is_empty(vars_dropped)){
    
    data_trn[, cnst] = NULL
    data[, cnst] = NULL
    
    cols <- cols %>% 
      add_row(
        description = "non-static variables",
        count = ncol(data),
        dropped = glue_collapse(vars_dropped, sep =', ', last = ', and ')
      )
    
  }
  
  additional_constant_variables <- data_trn %>% 
    map_int(~length(unique(na.omit(.x)))) %>% 
    enframe() %>% 
    filter(value == 1) %>% 
    pull(name)
  
  if(!is_empty(additional_constant_variables)){
    
    data_trn[, additional_constant_variables] = NULL
    data[, additional_constant_variables] = NULL
    
    cols <- cols %>% 
      add_row(
        description = "static after removing missing variables",
        count = ncol(data),
        dropped = glue_collapse(additional_constant_variables, 
          sep =', ', last = ', and ')
      )
    
  }
  
  dbls <- whichAreInDouble(data_trn, verbose = TRUE)
  vars_dropped <- names(data_trn)[dbls]
  
  if(!is_empty(vars_dropped)){
    
    data[, dbls] = NULL
    data_trn[, dbls] = NULL
    
    cols <- cols %>% 
      add_row(
        description = "non-duplicated variables",
        count = ncol(data),
        dropped = glue_collapse(vars_dropped, sep =', ', last = ', and ')
      )
    
  }
  
  vars_to_keep <- data_trn %>%
    summarize_all(~mean(is.na(.x))) %>%
    gather(variable, miss_perc) %>%
    filter(miss_perc <= 0.90) %>%
    pull(variable)
  
  vars_dropped <- setdiff(names(data_trn), vars_to_keep)
  
  
  data_dropped_miss <- data %>%
    select_at(vars_to_keep)
  
  cols <- cols %>%
    add_row(
      description = "variables with < 90% missing",
      count = ncol(data_dropped_miss),
      dropped = glue_collapse(vars_dropped, sep =', ', last = ', and ')
    )
  
  data_derived_vars <- data_dropped_miss %>% 
    as_tibble() %>% 
    mutate(
      ccs = as.numeric(m0_px_profile == 'x1_critical_cardiogenic_shock'),
      current_smoker = if_else(
        m0_cc_curr_smoking_m == 'yes' | m0_cc2_curr_smoking_m == 'yes',
        true = 1, 
        false = 0
      ),
      periph_vasc = if_else(
        m0_cc_periph_vasc_disease_m == 'yes' | 
          m0_cc2_periph_vasc_disease_m == 'yes',
        true = 1, 
        false = 0
      ),
      non_comp = if_else(
        m0_cc2_rptd_non_compliance_m == 'yes' | 
          m0_cc_rptd_non_compliance_m == 'yes',
        true = 1, 
        false = 0
      ),
      age2 = m0_age_deident^2,
      bmi = m0_wgt_kg / (m0_hgt_cm/100)^2
    )
  
  binary_indicators <- select_if(data_derived_vars, is_indicator) %>% 
    dplyr::select(-starts_with('pt_outcome'),-ends_with("count")) %>% 
    names()
  
  data_out <- data_derived_vars %>% 
    filter(m0_impl_yr >= min(train_years), pt_outcome_months > 0) %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_at(
      tidyselect::all_of(binary_indicators), 
      factor, 
      levels = c(0,1), 
      labels = c("no", "yes")
    ) 
  
  list(
    data = data_out,
    cols_dropped = cols
  )

}
