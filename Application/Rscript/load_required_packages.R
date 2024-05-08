# Function to check and install missing packages, then load them
load_required_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      message(paste("Package", package, "is not installed. Attempting to install now."))
      install.packages(package)
    }
    # Load the package after installation or if it was already installed
    library(package, character.only = TRUE)
  }
}