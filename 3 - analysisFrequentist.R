# ------------------------ #
## Frequentist Regression ##
# ------------------------ #

# ----------------------------------------------------------------
# Desc: Carry out frequentist multi-linear regression.
# Naming convention: N/A
# Credit: None
# Script Dependencies:
  # 1. '0.1 - functions.R'
  # 2. '1.1 - dataLoad.R'
  # 3. '1.2 - dataWrangle.R'
  # 4. '1.3 - dataConsolidation.R'
  # 5. '2.1 - dataExploration.R'
# Packages Used: caret
# Notes: None
# ----------------------------------------------------------------

# Set random number generator seed to a number to reproduce results
set.seed(123)
# Set-up k-fold cross-validation
trainSet <- trainControl(method = "cv", number = 10)
# Run linear regression model
model_regFreq <- train(form = `Graduate Prospects 2018` ~ .,
                       data = mat_reg, trControl = trainSet,
                       method = "lm")
# Compute confidence interval, and clean look
rpt_ci <- confint(object = model_regFreq$finalModel, level = 0.95)
rpt_ci <- round(x = rpt_ci[,c("2.5 %","97.5 %")], digits = 2)
row.names(rpt_ci) <- gsub(pattern = "\\\\|`", replacement = "", x = row.names(rpt_ci))