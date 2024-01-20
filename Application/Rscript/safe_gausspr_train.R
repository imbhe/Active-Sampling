safe_gausspr_train <- function(...) { 
  purrr::safely(kernlab::gausspr, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}