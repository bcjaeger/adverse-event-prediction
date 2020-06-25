

# source("packages.R")
# for(f in list.files('R', full.names = T)) source(f)

# debug(make_train_test)

the_plan <-
  drake_plan(
    
    train_years = 2012:2016,
    time_cutpoint = max(train_years),
    outcome_colname = 'm1_bleed_ae_count',
    
    visit0 = clean_data(visit = 0, train_years = train_years),
    visit1 = clean_data(visit = 1, train_years = train_years),
    visit2 = clean_data(visit = 2, train_years = train_years),
    
    data_all = make_analysis_data(
      pre = visit1, 
      post = visit2, 
      train_years = train_years, 
      outcome_colname = outcome_colname
    ),
    
    data_train = filter(data_all, m0_impl_yr %in% train_years),
    data_test = filter(data_all, m0_impl_yr > time_cutpoint),
    data_folds = group_vfold_cv(data = data_train, group = 'm0_impl_yr'),
    
    recipe = make_recipe(data_train),
    
    # xgb_params = tune_xgb_params(data_folds = data_folds, recipe = recipe),
    xgb_params = list(
      eta = 0.02, 
      gamma = 5, 
      max_depth = 4,
      min_child_weight = 5,
      subsample = 1/2, 
      colsample_bynode = 1/2,
      num_parallel_tree = 2,
      eval_metric = 'auc'
    ),
    
    preproc_steps = prep(recipe),
    preproc_train = juice(preproc_steps),
    preproc_test = bake(preproc_steps, new_data = data_test),
    
    xgb_fit_final = fit_xgb(preproc_train, data_folds, preproc_test, xgb_params),
    
    # final_xgb_fit = xgb_fit(data = preproc_train, params = xgb_params),
    # final_xgb_prd_prob = xgb_predict_prob(data = preproc_test),
    # final_xgb_prd_shap = xgb_predict_shap(data = preproc_test),
    
    report = target(
      command = {
        rmarkdown::render(knitr_in("doc/analysis.Rmd"))
        file_out("doc/analysis.html")
      }
    )

)

#make(the_plan)
