
library(seminr)
library(tidyverse)

# Data preparation 
data_model3 <- data_treated

#AI-KM-TP

# Specify measurement model with interaction terms
measurement_model3 <- constructs(
  composite("KT", multi_items("KT", 1:4)),
  composite("KA", multi_items("KA", 1:3)),
  composite("TP", multi_items("TP", 1:7)),
  composite("AI", multi_items("AI", 1:8))
)

# Specify structural model
structural_model3 <- relationships(
  paths(from = "KA", to = "TP"),
  paths(from = "KT", to = "TP"),
  paths(from = "AI", to = c("KT", "KA", "TP"))
)

# Estimate PLS-SEM model
pls_model3 <- estimate_pls(
  data = data_treated,
  measurement_model = measurement_model3,
  structural_model = structural_model3
)

#######Measurement Model Analysis

#Summarize the results of the model estimation
model_summary_model3 <- summary(pls_model3)

#Factor loadings
model_summary_model3$loadings

#Reliability result table
model_summary_model3$reliability

#HTMT result table
model_summary_model3$validity$htmt

#######Structural Model Analysis

# Bootstrap the model
boot_model3 <- bootstrap_model(seminr_model = pls_model3, nboot = 5000)

#Summarize the results of the bootstrap
boot_summary_model3 <- summary(boot_model3, alpha = 0.05)

#Assess collinearity issues
model_summary_model3$vif_antecedents

#Assess Path Coefficients
print(boot_summary_model3$bootstrapped_paths)

#Assess RSquares
model_summary_model3$paths

# Calculate and print f² effect sizes
f2_values <- boot_summary_model3fSquare
print("f² effect sizes:")
print(f2_values)

#Mediation Analysis: 
model_summary_model3$total_indirect_effects

specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "KA",
                             to = "TP",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "KT",
                             to = "TP",
                             alpha = 0.05)

model_summary_model3$paths["AI", "TP"]*
  model_summary_model3$paths["AI", "KA"]*
  model_summary_model3$paths["KA", "TP"]

