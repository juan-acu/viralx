test_that("`glob_lasso_vis()` plots as expected", {
  library(qvirus)
  library(dplyr)
  
  set.seed(123)
  
  hiv_data <- qphen[, -1]
  lasso_hyperparameters <- list(penalty = 0.01)
  vip_featured <- "cd_diff"
  vip_train <- hiv_data |>
  dplyr::select(-all_of(vip_featured))
  v_train <- hiv_data |>
  dplyr::select(all_of(vip_featured))
  vdiffr::expect_doppelganger(
    title = "global cr vis",
    fig = glob_lasso_vis(vip_featured,hiv_data,lasso_hyperparameters,vip_train,v_train)
  )
})
