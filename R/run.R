
# Environment ---------------------------------------------------------------------------------

# Install packages
source("R/env.R")

# Model training on original data of Transplant Int 2018 --------------------------------------

# Exploratory data analysis for the training data
source("R/func.R")
source("R/eda.R")

# Model 'training' for original, recalibrated and retrained models
source("R/train_model.R")

# Report performance in training data
source("R/func.R")
source("R/report_model.R")


# Model validation on new data ----------------------------------------------------------------

# Extract, Transform and Load the validation data
source("R/etl.R")

# Exploratory data analysis for validation data
source("R/func.R")
source("R/eda_val_data.R")

# Validate the model
source("R/func.R")
source("R/validate_model.R")

# Consume the model ----
shiny::runApp("R/predictions.R")


