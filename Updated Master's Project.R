# Loading packages
library(readxl)
library(tidyverse)
library(gtsummary)
library(gt)
library(dplyr)

# Download the data
homeless_data_1 <- read_excel("Downloads/Master's Project/Structural Data 2016 to 2022 (2).xlsx")

# List of independent variables for bivariate regressions
variables <- c("MedianRent1Bed", "MedianContractRent", "LowIncomeRentBurden", 
               "HomeValue", "MedianIncome", "OwnershipRate", "VacancyRate", 
               "UnemploymentRate", "PovertyRate", "BlackDemo", "HispanicDemo", 
               "BabyBoomer", "OnePersonHousehold", "PublicAssistance", 
               "SocialSecurityBenefits", "IncomeInequality", "MobilityRate")

# Initialize a list to store results for overall bivariate regression
bi_overall_ols_results <- list()

# Loop over each variable to run bivariate OLS regressions
for (var in variables) {
  formula <- as.formula(paste("HomelessRate ~", var))
  model <- lm(formula, data = homeless_data_1)  # Use lm() for OLS regression
  bi_overall_ols_results[[var]] <- summary(model)
}

# Print results
for (var in variables) {
  cat("\n\n### Bivariate OLS Regression for", var, "###\n")
  print(bi_overall_ols_results[[var]])
}

# Rural data
homeless_data_rural <- homeless_data_1[homeless_data_1$OFMRuralUrban == "Rural", ]

# Initialize a list to store results for rural bivariate regression
bi_rural_ols_results <- list()

# Loop over each variable to run bivariate OLS regressions
for (var in variables) {
  formula <- as.formula(paste("HomelessRate ~", var))
  model <- lm(formula, data = homeless_data_rural)  # Use lm() for OLS regression
  bi_rural_ols_results[[var]] <- summary(model)
}

# Print results
for (var in variables) {
  cat("\n\n### Bivariate OLS Regression for", var, "###\n")
  print(bi_rural_ols_results[[var]])
}


# Dummy Variables to control for years (fixed effects)
# Load necessary libraries
library(dplyr)
library(MASS)

#Year as dummy variables for both overall washington counties
homeless_data_1$Year2017<-ifelse(homeless_data_1$year == 2017, 1, 0)
homeless_data_1$Year2018<-ifelse(homeless_data_1$year == 2018, 1, 0)
homeless_data_1$Year2019<-ifelse(homeless_data_1$year == 2019, 1, 0)
homeless_data_1$Year2020<-ifelse(homeless_data_1$year == 2020, 1, 0)
homeless_data_1$Year2022<-ifelse(homeless_data_1$year == 2022, 1, 0)

# Lagged Dependent variable
library(dplyr)

# Try creating lagged HomelessRate without grouping first
homeless_data_lag <- homeless_data_1 %>%
  arrange(county, year) %>%
  group_by(county) %>%
  mutate(HomelessRate_Lag = dplyr::lead(HomelessRate)) %>%
  ungroup()

# MODEL 1 - ALL VARIABLES
# Run the regression using the lagged HomelessRate as the dependent variable
model1_lag <- lm(HomelessRate_Lag ~ MedianRent1Bed + MedianContractRent + 
                   LowIncomeRentBurden + HomeValue + MedianIncome + OwnershipRate +
                   VacancyRate + UnemploymentRate + PovertyRate + BlackDemo +
                   HispanicDemo + BabyBoomer + OnePersonHousehold +
                   PublicAssistance + SocialSecurityBenefits + IncomeInequality +
                   MobilityRate+ Year2017 + Year2018 + Year2019 + Year2020 + 
                   Year2022, data = homeless_data_lag)

# Display the summary of the model
model1_lag <- summary(model1_lag)
# Round each coefficient for a cleaner output
model1_lag$coefficients[, 1] <- round(model1_lag$coefficients[, 1], 3)  # Round estimates
model1_lag$coefficients[, 2] <- round(model1_lag$coefficients[, 2], 3)  # Round standard errors
model1_lag$coefficients[, 3] <- round(model1_lag$coefficients[, 3], 3)  # Round t-values
model1_lag$coefficients[, 4] <- round(model1_lag$coefficients[, 4], 3)  # Round p-values

# Print the rounded summary
print(model1_lag)

# Calculate the Mean Squared Error (MSE)
mse1 <- mean(residuals(model1_lag)^2)
cat("Mean Squared Error (MSE) of the best model:", mse1, "\n")


# MODEL 2 - Bivariate & Correlation Analysis
model2_lag <- lm(HomelessRate_Lag ~ MedianRent1Bed + PovertyRate + 
                   BabyBoomer + OnePersonHousehold + MobilityRate + Year2017 + 
                   Year2018 + Year2019 + Year2020 + Year2022, data = homeless_data_lag)

# Display the summary of the model
model2_lag <- summary(model2_lag)

# Round each coefficient for a cleaner output
model2_lag$coefficients[, 1] <- round(model2_lag$coefficients[, 1], 3)  # Round estimates
model2_lag$coefficients[, 2] <- round(model2_lag$coefficients[, 2], 3)  # Round standard errors
model2_lag$coefficients[, 3] <- round(model2_lag$coefficients[, 3], 3)  # Round t-values
model2_lag$coefficients[, 4] <- round(model2_lag$coefficients[, 4], 3)  # Round p-values

# Print the rounded summary
print(model2_lag)

# Calculate the Mean Squared Error (MSE)
mse2 <- mean(residuals(model2_lag)^2)
cat("Mean Squared Error (MSE) of the best model:", mse2, "\n")

# MODEL 3 - AIC/BIC
library(MASS)

# Define the full model with all variables
model3_lagg <- lm(HomelessRate_Lag ~ MedianRent1Bed + LowIncomeRentBurden + OwnershipRate +
                   VacancyRate + UnemploymentRate + PovertyRate + BlackDemo +
                   HispanicDemo + BabyBoomer + OnePersonHousehold +
                   PublicAssistance + SocialSecurityBenefits + IncomeInequality +
                   MobilityRate + Year2017 + Year2018 + Year2019 + Year2020 +
                   Year2022, data = homeless_data_lag)

# Display the summary of the model
model3_lagg <- summary(model3_lagg)

# Round each coefficient for a cleaner output
model3_lagg$coefficients[, 1] <- round(model3_lagg$coefficients[, 1], 3)  # Round estimates
model3_lagg$coefficients[, 2] <- round(model3_lagg$coefficients[, 2], 3)  # Round standard errors
model3_lagg$coefficients[, 3] <- round(model3_lagg$coefficients[, 3], 3)  # Round t-values
model3_lagg$coefficients[, 4] <- round(model3_lagg$coefficients[, 4], 3)  # Round p-values

# Perform stepwise selection based on AIC
best_model_aic <- stepAIC(model3_lagg, direction = "both", trace = FALSE)

# Display the summary of the best model based on AIC
summary(best_model_aic)

# Calculate the Mean Squared Error (MSE)
mse3 <- mean(residuals(best_model_aic)^2)
cat("Mean Squared Error (MSE) of the best model:", mse3, "\n")


# Model 3
model3_manual <-lm(HomelessRate_Lag ~ MedianRent1Bed + MedianContractRent + LowIncomeRentBurden + 
                     HomeValue + MedianIncome + OwnershipRate +
                     VacancyRate + UnemploymentRate + PovertyRate + BlackDemo +
                     HispanicDemo + BabyBoomer + OnePersonHousehold +
                     PublicAssistance + SocialSecurityBenefits + IncomeInequality +
                     MobilityRate + Year2017 + Year2018 + Year2019 + Year2020 +
                     Year2022, data = homeless_data_lag)

# Stepwise selection
best_model_aic <- stepAIC(model3_manual, direction = "both", trace = FALSE)
summary(best_model_aic)

# Calculate the Mean Squared Error (MSE)
mse3 <- mean(residuals(best_model_aic)^2)
cat("Mean Squared Error (MSE) of the best model:", mse3, "\n")

# Model 3
model3_manual_1 <-lm(HomelessRate_Lag ~ MedianRent1Bed + LowIncomeRentBurden + OwnershipRate +
                       VacancyRate + UnemploymentRate + PovertyRate + BlackDemo +
                       HispanicDemo + BabyBoomer + OnePersonHousehold +
                       PublicAssistance + SocialSecurityBenefits + IncomeInequality +
                       MobilityRate + Year2017 + Year2018 + Year2019 + Year2020 +
                       Year2022, data = homeless_data_lag)

# Stepwise selection
best_model_aic_1 <- stepAIC(model3_manual_1, direction = "both", trace = FALSE)
summary(best_model_aic_1)

# Calculate the Mean Squared Error (MSE)
mse3 <- mean(residuals(best_model_aic_1)^2)
cat("Mean Squared Error (MSE) of the best model:", mse3, "\n")
