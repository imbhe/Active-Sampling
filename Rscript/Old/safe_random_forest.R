safe_random_forest <- function(...) { 
  purrr::safely(randomForest::randomForest, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}