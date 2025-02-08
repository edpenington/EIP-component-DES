# setup.R

# Install renv if not already installed
if (!require("renv")) {
  install.packages("renv")
}

# Install required packages
renv::restore()
