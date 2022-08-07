
# Environment ---------------------------------------------------------------------------------

# Install packages
source("R/env.R")

# Model training on original data of Transplant Int 2018 --------------------------------------
install.packages("here")
library(here)

# Exploratory data analysis for the training data
source(here("R", "func.R"))
source(here("R", "eda.R"))

# Model 'training' for original, recalibrated and retrained models
source(here("R","train_model.R"))

# Report performance in training data
source(here("R","func.R"))
source(here("R", "report_model.R"))

# Model validation on new data ----------------------------------------------------------------

# Extract, Transform and Load the validation data
source(here("R","etl.R"))

# Exploratory data analysis for validation data
source(here("R","func.R"))
source(here("R", "eda_val_data.R"))

# Validate the model
source(here("R", "func.R"))
source(here("R", "validate_model.R"))

# Consume the model ----
shiny::runApp(here("R", "predictions.R"))

# Additional figures
source(here("R", "cuminc_rumc.R"))

