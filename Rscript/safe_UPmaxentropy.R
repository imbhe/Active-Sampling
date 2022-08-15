safe_UPmaxentropy <- function(...) { 
  purrr::safely(sampling::UPmaxentropy, otherwise = NULL, quiet = TRUE)(...)[["result"]]
}