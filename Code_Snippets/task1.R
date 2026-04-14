# ==============================================================================
# File 3: task1.R
# Page 6, Question 1: Factor Calibration and Scenario Design (20%)
# ==============================================================================

# ==============================================================================
# 1. DATA PREPARATION & WINDOWING (Task 1a)
# ==============================================================================
print("Task 1a: Preparing Data and Defining Windows...")

# Merge factors and levels
merged_data <- monthly_factors_base_df %>%
  inner_join(monthly_levels_base_df, by = "date") %>%
  mutate(date = as.Date(date)) %>%
  drop_na()

# Create approximately 5-year windows
merged_data <- merged_data %>%
  mutate(
    year = year(date),
    window = case_when(
      year <= 2005 ~ "Window 1: 2001-2005",
      year >= 2006 & year <= 2010 ~ "Window 2: 2006-2010",
      year >= 2011 & year <= 2015 ~ "Window 3: 2011-2015",
      year >= 2016 & year <= 2020 ~ "Window 4: 2016-2020",
      year >= 2021 ~ "Window 5: 2021-2026"
    )
  )

# Count datapoints per specific window (just to verify how many datapoints i have)
##window_audit <- merged_data %>%
##  group_by(window) %>%
##  summarise(
##    datapoints = n(),
##    start_date = min(date),
##    end_date = max(date),
##    completeness_pct = round((n() / 60) * 100, 1) # Assuming 5-year targets
##  ) %>%
##  arrange(start_date)

# View the result
##print(window_audit)

# ==============================================================================
# 2. KPI CALCULATION & ANALYSIS (Task 1a & 1b)
# Generate the stats so the user can justify their scenario choice
# ==============================================================================
cat("\n--- WINDOW KPI ANALYSIS (Means, Volatilities, Fat Tails) ---\n")
window_kpis <- merged_data %>%
  group_by(window) %>%
  summarize(
    # AI Returns
    ai_mean = mean(software_ai_return),
    ai_sd = sd(software_ai_return),
    ai_kurtosis = kurtosis(software_ai_return) - 3,
    
    # Oil Returns
    oil_mean = mean(oil_return),
    oil_sd = sd(oil_return),
    oil_kurtosis = kurtosis(oil_return) - 3,
    
    # Rate Changes (bps)
    ## when looking at the optimal model for simulation
    ## we dont use the bps format but the normal decimal
    ## version (human vs machine readability)
    rate_mean_bps = mean(rate_change_5y_bps),
    rate_sd_bps = sd(rate_change_5y_bps),
    rate_kurtosis = kurtosis(rate_change_5y_bps) - 3
  )

# Print the full table neatly
print(as.data.frame(window_kpis))

cat("\n--- CORRELATION MATRICES PER WINDOW ---\n")
windows <- unique(merged_data$window)
cor_matrices <- list()

for(w in windows) {
  w_data <- merged_data %>% filter(window == w) %>%
    select(software_ai_return, oil_return, rate_change_5y)
  cor_matrices[[w]] <- cor(w_data)
  
  cat(sprintf("\n%s\n", w))
  print(round(cor_matrices[[w]], 3))
}

print("Generating Distribution Visualizations...")

# Prepare data for plotting (long format)
plot_data <- merged_data %>%
  select(date, window, software_ai_return, oil_return, rate_change_5y_bps) %>%
  pivot_longer(cols = c(software_ai_return, oil_return, rate_change_5y_bps), 
               names_to = "Asset", values_to = "Value")

# Plot Density Distributions faceted by Asset and Window
density_plot <- ggplot(plot_data, aes(x = Value, fill = window)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Asset, scales = "free", ncol = 1) +
  theme_minimal() +
  labs(title = "Distribution of Market Factors Across 5-Year Windows",
       subtitle = "Observe fat tails and variance expansion during stress periods (e.g., GFC)",
       x = "Monthly Change / Return", y = "Density") +
  theme(legend.position = "bottom")

# Save plot to your working directory
ggsave("visualization/factor_distributions.png", plot = density_plot, width = 10, height = 8)
cat("-> Saved distribution visualization as 'factor_distributions.png'\n")

# ==============================================================================
# 3. USER CONFIGURATION: SCENARIO SELECTION 
# (Based on the printed KPIs above, select your windows here)
# ==============================================================================
# Page 6, Question 1b: Construct and justify a stress scenario

benchmark_window_name <- "Window 3: 2011-2015"
stress_window_name <- "Window 2: 2006-2010"

# Dynamically extract the chosen correlation structures
cor_bench <- cor_matrices[[benchmark_window_name]]
cor_stress <- cor_matrices[[stress_window_name]]

cat(sprintf("\n=> Selected BENCHMARK: %s\n", benchmark_window_name))
cat(sprintf("=> Selected STRESS: %s\n", stress_window_name))

# ==============================================================================
# 4. STATISTICAL DIAGNOSTIC FOR DEPENDENCE STRUCTURE
# ==============================================================================
## monte carlo simulations need stationary data
target_vars <- c("software_ai_return", "oil_return", "rate_change_5y")
fat_tail_votes <- 0

cat("\n[DIAGNOSTIC] Checking for Fat Tails (Kurtosis) and Autocorrelation across full dataset...\n")
for (var in target_vars) {
  ret_data <- merged_data[[var]]
  ex_kurt <- kurtosis(ret_data) - 3
  lb_test <- Box.test(ret_data, lag = 1, type = "Ljung-Box")
  
  ## think we can all agree that perfect normality doesnt exist in
  ## financial markets. according to AI a rule of thumb is 1.0 as 
  ## a threshhold
  if (ex_kurt > 1.0) fat_tail_votes <- fat_tail_votes + 1
  ## ljung box checks for 95% confidence interval autocorrellation 
  if (lb_test$p.value < 0.05) fat_tail_votes <- fat_tail_votes + 1
}

if (fat_tail_votes >= 2) {
  cat("-> AI/DIAGNOSTIC CONCLUSION: Excess kurtosis detected. GARCH(1,1) / t-Copula recommended to capture tail risks.\n\n")
} else {
  cat("-> AI/DIAGNOSTIC CONCLUSION: Uniform data profile. Linear / Gaussian Copula recommended.\n\n")
}

# ==============================================================================
# 5. TASK 1c: SIMULATION ENGINE (LINEAR vs GARCH/T-COPULA TOGGLE)
# ==============================================================================
print("Task 1c: Simulating 60-month paths...")

# --- USER TOGGLE FOR SIMULATION TYPE ---
sim_mode <- "GARCH_T_COPULA" # Options: "LINEAR" or "GARCH_T_COPULA"
df_t <- 5 # Degrees of freedom for Student-t Copula (lower = fatter tails)

n_sims <- 1000
n_months <- horizon_months # Inherited from main.R

# Arrays to hold Z-scores [Simulations, Months, Factors]
z_bench_array <- array(NA, dim = c(n_sims, n_months, 3))
z_stress_array <- array(NA, dim = c(n_sims, n_months, 3))

chol_bench <- chol(cor_bench)
chol_stress <- chol(cor_stress)

set.seed(2026) 

for (i in 1:n_sims) {
  # Step 1: Draw independent standard normal variables N(0,1)
  Z_indep_bench <- matrix(rnorm(n_months * 3), ncol = 3)
  Z_indep_stress <- matrix(rnorm(n_months * 3), ncol = 3)
  
  if (sim_mode == "LINEAR") {
    # --------------------------------------------------------------------------
    # LINEAR / GAUSSIAN COPULA (Standard Cholesky mapping)
    # --------------------------------------------------------------------------
    z_bench_array[i, , ] <- Z_indep_bench %*% chol_bench
    z_stress_array[i, , ] <- Z_indep_stress %*% chol_stress
    
  } else if (sim_mode == "GARCH_T_COPULA") {
    # --------------------------------------------------------------------------
    # GARCH / STUDENT-T COPULA (Chi-Square Mixture approach)
    # --------------------------------------------------------------------------
    # Draw shared Chi-Square variable for joint tail-events (systemic shocks)
    chi_bench <- rchisq(n_months, df = df_t)
    chi_stress <- rchisq(n_months, df = df_t)
    
    # Mix the Normal variables with the Chi-Square to create t-distributed Z-scores
    Z_t_bench <- Z_indep_bench * sqrt(df_t / chi_bench)
    Z_t_stress <- Z_indep_stress * sqrt(df_t / chi_stress)
    
    # Induce correlation via Cholesky
    z_bench_array[i, , ] <- Z_t_bench %*% chol_bench
    z_stress_array[i, , ] <- Z_t_stress %*% chol_stress
  }
}

cat(sprintf("Successfully generated %d simulated paths using %s mode.\n", n_sims, sim_mode))

# ==============================================================================
# OPTIONAL VISUALIZATION: Proving Tail Dependence & Crisis Co-Movement
# ==============================================================================
print("Generating Scatter Plot to prove Tail Dependence...")

# Flatten the 3D arrays into 2D dataframes for plotting
# We will just look at AI (Factor 1) vs Oil (Factor 2)
df_bench_sim <- data.frame(
  AI_Shock = as.vector(z_bench_array[,,1]),
  Oil_Shock = as.vector(z_bench_array[,,2]),
  Scenario = "Benchmark (Normal Copula)"
)

df_stress_sim <- data.frame(
  AI_Shock = as.vector(z_stress_array[,,1]),
  Oil_Shock = as.vector(z_stress_array[,,2]),
  Scenario = "Stress (t-Copula + Crisis Correlation)"
)

df_plot_sim <- rbind(df_bench_sim, df_stress_sim)

# Scatter plot showing joint extremes
copula_plot <- ggplot(df_plot_sim, aes(x = AI_Shock, y = Oil_Shock, color = Scenario)) +
  geom_bin2d(bins = 60) +
  facet_wrap(~Scenario) +
  theme_minimal() +
  labs(title = "Simulated Joint Shocks: Benchmark vs Stress",
       subtitle = "Notice the extreme joint-negative shocks (bottom-left) in the Stress scenario",
       x = "Simulated AI Shock (Z-Score)",
       y = "Simulated Oil Shock (Z-Score)") +
  scale_color_manual(values = c("Benchmark (Normal Copula)" = "blue", 
                                "Stress (t-Copula + Crisis Correlation)" = "red")) +
  theme(legend.position = "none")

ggsave("visualization/copula_tail_dependence.png", plot = copula_plot, width = 10, height = 5)
cat("-> Saved simulation scatter plot as 'copula_tail_dependence.png'\n")

# ==============================================================================
# 6. ENVIRONMENT CLEANUP
# ==============================================================================
print("Task 1 Complete. Cleaning up temporary environment variables...")

# Remove temporary dataframes, lists, and diagnostic variables
rm(merged_data, cor_matrices, windows, w, w_data, target_vars, 
   fat_tail_votes, var, ret_data, ex_kurt, lb_test)

# Remove temporary parameters and correlation matrices
rm(cor_bench, cor_stress, chol_bench, chol_stress, benchmark_window_name, 
   stress_window_name, sim_mode, df_t)

# Remove loop simulation vectors
rm(i, Z_indep_bench, Z_indep_stress)

# Safely suppress warnings and remove t-copula specific loop variables if they exist
suppressWarnings(rm(Z_t_bench, Z_t_stress, chi_bench, chi_stress))

print("Environment cleaned. Ready for Task 2.")