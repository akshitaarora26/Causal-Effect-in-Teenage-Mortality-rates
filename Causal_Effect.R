# Load necessary libraries
library(haven)
library(ggplot2)

# Load the data
file_path <- "all.dta"  # Update this path if needed
all_data <- haven::read_dta("https://raw.githubusercontent.com/aamish29/bigdata/main/all.dta")

# Question 1: Calculate mortality rates for individuals above and below the MLDA
# Filter data
above_mlda <- subset(all_data, agemo_mda >= 1 & agemo_mda <= 24)
below_mlda <- subset(all_data, agemo_mda <= -1 & agemo_mda >= -24)

# Mortality rate formula
above_mlda_rate <- 100000 * sum(above_mlda$cod_any) / (sum(above_mlda$pop) / 12)
below_mlda_rate <- 100000 * sum(below_mlda$cod_any) / (sum(below_mlda$pop) / 12)

# Print results
cat("Mortality rate (1-24 months above MLDA):", above_mlda_rate, "\n")
cat("Mortality rate (1-24 months below MLDA):", below_mlda_rate, "\n")


# Question 2: Create scatter plot for mortality rates
# Filter data for individuals within 2 years of MLDA
within_2_years <- subset(all_data, agemo_mda >= -24 & agemo_mda <= 24)

# Calculate mortality rates
within_2_years$mortality_any <- 100000 * within_2_years$cod_any / (within_2_years$pop / 12)
within_2_years$mortality_MVA <- 100000 * within_2_years$cod_MVA / (within_2_years$pop / 12)

# Plotting
plot <- ggplot(data = within_2_years, aes(x = agemo_mda)) +
  geom_point(aes(y = mortality_any), color = "black", shape = 15, size = 2, alpha = 0.7) +
  geom_point(aes(y = mortality_MVA), color = "blue", shape = 16, size = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Mortality Rates by Age Relative to MLDA",
       x = "Age (months relative to MLDA)",
       y = "Mortality Rate (per 100,000 person-years)") +
  theme_minimal() +
  theme(legend.position = "top")

print(plot)


# Question 3: Non-parametric "donut" RD estimation
# Exclude partially-treated observations (agemo_mda == 0)
data_donut <- subset(all_data, agemo_mda != 0)

# Define bandwidths
bandwidths <- c(48, 24, 12, 6)

# Initialize result storage for non-parametric RD
nonparam_results <- data.frame(
  Bandwidth = bandwidths,
  RD_Estimate_AllCause = numeric(length(bandwidths)),
  RD_Estimate_MVA = numeric(length(bandwidths))
)

# Loop through bandwidths for RD estimation
for (i in seq_along(bandwidths)) {
  bw <- bandwidths[i]
  subset_data <- subset(data_donut, abs(agemo_mda) <= bw)
  
  # Non-parametric linear regression for all-cause mortality
  fit_any <- lm(100000 * cod_any / (pop / 12) ~ I(agemo_mda >= 0) + agemo_mda, data = subset_data)
  nonparam_results$RD_Estimate_AllCause[i] <- coef(fit_any)["I(agemo_mda >= 0)TRUE"]
  
  # Non-parametric linear regression for motor vehicle accident mortality
  fit_MVA <- lm(100000 * cod_MVA / (pop / 12) ~ I(agemo_mda >= 0) + agemo_mda, data = subset_data)
  nonparam_results$RD_Estimate_MVA[i] <- coef(fit_MVA)["I(agemo_mda >= 0)TRUE"]
}

# Print non-parametric RD results
cat("Non-parametric RD Results:\n")
print(nonparam_results)


# Question 4: Parametric "donut" RD estimation
# Initialize result storage for parametric RD
param_results <- data.frame(
  Bandwidth = bandwidths,
  RD_Estimate_AllCause = numeric(length(bandwidths)),
  RD_Estimate_MVA = numeric(length(bandwidths))
)

# Loop through bandwidths for parametric RD estimation
for (i in seq_along(bandwidths)) {
  bw <- bandwidths[i]
  subset_data <- subset(data_donut, abs(agemo_mda) <= bw)
  
  # Parametric linear regression for all-cause mortality
  fit_any_param <- lm(100000 * cod_any / (pop / 12) ~ I(agemo_mda >= 0) * agemo_mda, data = subset_data)
  param_results$RD_Estimate_AllCause[i] <- coef(fit_any_param)["I(agemo_mda >= 0)TRUE"]
  
  # Parametric linear regression for motor vehicle accident mortality
  fit_MVA_param <- lm(100000 * cod_MVA / (pop / 12) ~ I(agemo_mda >= 0) * agemo_mda, data = subset_data)
  param_results$RD_Estimate_MVA[i] <- coef(fit_MVA_param)["I(agemo_mda >= 0)TRUE"]
}

# Print parametric RD results
cat("\nParametric RD Results:\n")
print(param_results)