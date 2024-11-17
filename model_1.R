
library(seminr)
library(tidyverse)

# Data preparation 
data_model3 <- data_treated

#AI_TMS_KM

# Specify measurement model with interaction terms
measurement_model3 <- constructs(
  composite("SPEC", multi_items("SPEC", 1:5)),
  composite("CRED", multi_items("CRED", 1:3)),
  composite("COO", c("COO1","COO2","COO4")),
  composite("KT", multi_items("KT", 1:4)),
  composite("KA", multi_items("KA", 1:3)),
  composite("AI", multi_items("AI", 1:8))
)

# Specify structural model
structural_model3 <- relationships(
  paths(from = c("SPEC", "CRED", "COO"), to = c("KA", "KT")),
  paths(from = "AI", to = c("SPEC", "CRED", "COO")),
  paths(from = "AI", to = c("KA", "KT"))
)

# Estimate PLS-SEM model
pls_model3 <- estimate_pls(
  data = data_model3,
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
f2_values <- boot_summary_treated$fSquare
print("f² effect sizes:")
print(f2_values)

#Mediation Analysis: 
model_summary_model3$total_indirect_effects

specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "SPEC",
                             to = "KA",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "CRED",
                             to = "KA",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "COO",
                             to = "KA",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "SPEC",
                             to = "KT",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "CRED",
                             to = "KT",
                             alpha = 0.05)
specific_effect_significance(boot_model3, 
                             from = "AI",
                             through = "COO",
                             to = "KT",
                             alpha = 0.05)

model_summary_model3$paths["AI", "TP"]*
  model_summary_model3$paths["AI", "COO"]*
  model_summary_model3$paths["COO", "TP"]

