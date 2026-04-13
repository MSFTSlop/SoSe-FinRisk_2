# ==============================================================================
# File 5: task3.R
# Page 6, Question 3: Hedge Value and Counterparty Credit Risk (20%)
# ==============================================================================

# Note: Ensure main.R, formulas.R, task1.R, and task2.R are sourced and in the environment

# 1. EXTRACT PARAMETERS (Assuming these are cleanly preset in main.R)
# ==============================================================================
# If not explicitly in main.R, ensure these are loaded (Notional = 5m, Spread = 0.018, Rec = 0.6)
hedge_notional <- 5000000
fixed_spread <- 0.018

# We need the historical mean and SD to un-standardize the Z-scores 
# (Calculated from your monthly_factors_base_df)
mu_ai <- mean(monthly_factors_base_df$software_ai_return, na.rm = TRUE)
sd_ai <- sd(monthly_factors_base_df$software_ai_return, na.rm = TRUE)
mu_oil <- mean(monthly_factors_base_df$oil_return, na.rm = TRUE)
sd_oil <- sd(monthly_factors_base_df$oil_return, na.rm = TRUE)


# ==============================================================================
# Task 3a: Expected Discounted Value of Swap Payoffs
# ==============================================================================
print("Task 3a: Valuing Swap Payoffs...")

# Calculate Swap Payoffs based on the formula: Notional * (R_oil - R_ai - spread)
calc_swap_payoffs <- function(z_array) {
  n_sims <- dim(z_array)[1]
  payoff_matrix <- matrix(0, nrow = n_sims, ncol = horizon_months)
  
  for(t in 1:horizon_months) {
    # Reconstruct actual returns from the simulated Z-scores
    r_ai <- mu_ai + sd_ai * z_array[, t, 1]
    r_oil <- mu_oil + sd_oil * z_array[, t, 2]
    
    # Payoff execution
    payoff_matrix[, t] <- hedge_notional * (r_oil - r_ai - fixed_spread)
  }
  return(payoff_matrix)
}

payoff_bench <- calc_swap_payoffs(z_bench_array)
payoff_stress <- calc_swap_payoffs(z_stress_array)

# Calculate NPV of the standalone swap
calc_expected_swap_value <- function(payoff_matrix) {
  # Discount rate from main.R
  discount_factors <- 1 / (1 + risk_free/12)^(1:horizon_months) 
  
  # Apply discount factors to all paths and sum
  pv_matrix <- sweep(payoff_matrix, 2, discount_factors, "*")
  return(mean(rowSums(pv_matrix)))
}

swap_val_bench <- calc_expected_swap_value(payoff_bench)
swap_val_stress <- calc_expected_swap_value(payoff_stress)

cat(sprintf("Expected Swap Value (Benchmark): $%.2f\n", swap_val_bench))
cat(sprintf("Expected Swap Value (Stress):    $%.2f\n", swap_val_stress))


# ==============================================================================
# Task 3b: Computing CVA (Lecture 10 Logic)
# ==============================================================================
print("Task 3b: Computing CVA...")

# 1. Counterparty Default Probabilities (Lecture 10)
dealer_cds <- dealer_cds_curve_base_df$dealer_cds_spread

# Hazard rate: lambda = spread / (1 - R)
lambda_t <- dealer_cds / (1 - recovery_rate)

# Survival probability: S_t = exp(-lambda * t)
t_years <- (1:horizon_months) / 12
S_t <- exp(-lambda_t * t_years)

# Marginal default probability (q_t)
S_prev <- c(1, S_t[-horizon_months])
q_t <- S_prev - S_t

# 2. Expected Exposure & CVA Calculation
calc_cva <- function(payoff_matrix) {
  n_sims <- nrow(payoff_matrix)
  V_matrix <- matrix(0, nrow = n_sims, ncol = horizon_months)
  
  # Base Case: Value at maturity is just the final payoff
  V_matrix[, horizon_months] <- payoff_matrix[, horizon_months]
  
  # Backward Induction: Value at t = Payoff at t + PV(Value at t+1)
  for(t in (horizon_months - 1):1) {
    V_matrix[, t] <- payoff_matrix[, t] + V_matrix[, t+1] / (1 + risk_free/12)
  }
  
  # Exposure ONLY exists when V_t > 0 (Lecture 10 Rule)
  EE_t <- colMeans(pmax(V_matrix, 0))
  
  # Discount factors
  discount_factors <- 1 / (1 + risk_free/12)^(1:horizon_months)
  
  # Final CVA = Sum(LGD * PD * EE * DF)
  cva_value <- sum((1 - recovery_rate) * q_t * EE_t * discount_factors)
  return(cva_value)
}

cva_bench <- calc_cva(payoff_bench)
cva_stress <- calc_cva(payoff_stress)

cat(sprintf("Hedge CVA (Benchmark): $%.2f\n", cva_bench))
cat(sprintf("Hedge CVA (Stress):    $%.2f\n", cva_stress))


# ==============================================================================
# Task 3c: Net Hedge Value
# ==============================================================================
print("Task 3c: Calculating Net Hedge Value...")

net_bench <- swap_val_bench - cva_bench
net_stress <- swap_val_stress - cva_stress

cat(sprintf("Net Hedge Value (Benchmark): $%.2f\n", net_bench))
cat(sprintf("Net Hedge Value (Stress):    $%.2f\n", net_stress))

# ==============================================================================
# 4. ENVIRONMENT CLEANUP (End of Task 3)
# ==============================================================================
print("Task 3 Complete. Cleaning up temporary environment variables...")

# 1. Remove historical conversion metrics and swap parameters
# (Assuming hedge_notional, fixed_spread, and recovery_rate are in main.R, 
# we only remove the ones explicitly created in task3.R)
rm(mu_ai, sd_ai, mu_oil, sd_oil)

# 2. Remove intermediate CVA probability vectors
rm(dealer_cds, lambda_t, t_years, S_t, S_prev, q_t)

# 3. Remove Task 3 specific functions
rm(calc_swap_payoffs, calc_expected_swap_value, calc_cva)

# 4. Run garbage collection to free up memory from the large V_matrix inside the functions
gc()

cat("-> Environment cleaned. All final variables preserved. Ready for Task 4.\n")