safe_caret_train <- function(...) { 
  purrr::safely(caret::train, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}