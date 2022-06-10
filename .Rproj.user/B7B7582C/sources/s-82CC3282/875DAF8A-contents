safe_cv_glmnet <- function(...) { 
  purrr::safely(glmnet::cv.glmnet, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}