#' Global Visualization of SHAP Values for a Lasso Regression Model
#'
#' This function generates a visualization for the global feature importance of
#' Lasso regression model trained on HIV data with specified
#' hyperparameters.
#'
#' @param vip_featured The name of the response variable to explain.
#' @param hiv_data The training dataset containing predictor variables and the response variable.
#' @param lasso_hyperparameters A list of hyperparameters for the Lasso model, including:
#'   - \code{penalty}: Regularization parameter
#' @param vip_train The dataset used for training the Lasso model.
#' @param v_train The response variable used for training the Lasso model.
#'
#' @returns A visualization of global feature importance for the CR model.
#' @export
#'
#' @examples
#' \dontrun{
#' library(qvirus)
#' library(dplyr)
#'
#' set.seed(123)
#'
#' hiv_data <- qphen[, -1]
#'
#' lasso_hyperparameters <- list(
#'   penalty = 0.01
#' )
#'
#' vip_featured <- "cd_diff"
#'
#' vip_train <- hiv_data |>
#'   dplyr::select(-all_of(vip_featured))
#'
#' v_train <- hiv_data |>
#'   dplyr::select(all_of(vip_featured))
#'
#' glob_lasso_vis(
#'   vip_featured = vip_featured,
#'   hiv_data = hiv_data,
#'   lasso_hyperparameters = lasso_hyperparameters,
#'   vip_train = vip_train,
#'   v_train = v_train
)
#' }
glob_lasso_vis <- function(vip_featured, hiv_data, lasso_hyperparameters, vip_train, v_train) {
  
  # 1. Definir la fórmula dinámica (p. ej., "cd_2022 ~ .")
  formula_expr <- stats::as.formula(paste(vip_featured, "~."))
  
  # 2. Construir la receta (receta base con normalización)
  receta <- recipes::recipe(formula_expr, data = hiv_data) |> 
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_impute_mean(recipes::all_predictors()) |>
    recipes::step_normalize(recipes::all_predictors())
  
  # 3. Especificar el modelo Lasso (mixture = 1) usando glmnet
  espec_lasso <- parsnip::linear_reg(
    penalty = lasso_hyperparameters$penalty, 
    mixture = 1
  ) |> 
    parsnip::set_engine("glmnet") |> 
    parsnip::set_mode("regression")
  
  # 4. Ensamble e inserción en el flujo de trabajo (workflow)
  wflow <- workflows::workflow() |> 
    workflows::add_recipe(receta) |> 
    workflows::add_model(espec_lasso)
  
  # 5. Entrenar el flujo de trabajo
  fit_wflow <- parsnip::fit(wflow, data = hiv_data)
  
  # 6. Explicador de DALEXtra y gráfica de importancia de variables (model_parts)
  explainer <- DALEXtra::explain_tidymodels(
    fit_wflow, 
    data = vip_train, 
    y = v_train, 
    label = "lasso + normalized", 
    verbose = FALSE
  )
  
  plot(DALEX::model_parts(explainer))
}
