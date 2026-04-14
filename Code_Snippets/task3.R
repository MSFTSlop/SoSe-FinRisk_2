# ==============================================================================
# File 5: task3.R
# Page 6, Question 3: Hedge Value and Counterparty Credit Risk (20%)
# ==============================================================================
print("Starting Task 3: OTC Macro Hedge and CVA...")

# 1. EXTRACT PARAMETERS (Dynamically from Data)
# ==============================================================================

# Extract Swap Parameters dynamically from the dataframe loaded in main.R
hedge_notional <- as.numeric(swap_parameters_base_df$value[swap_parameters_base_df$parameter == "Notional"])
fixed_spread <- as.numeric(swap_parameters_base_df$value[swap_parameters_base_df$parameter == "Fixed_Spread"])

# We need the historical mean and SD to un-standardize the Z-scores back into actual returns
# (Calculated dynamically from your monthly_factors_base_df)
mu_ai <- mean(monthly_factors_base_df$software_ai_return, na.rm = TRUE)
sd_ai <- sd(monthly_factors_base_df$software_ai_return, na.rm = TRUE)
mu_oil <- mean(monthly_factors_base_df$oil_return, na.rm = TRUE)
sd_oil <- sd(monthly_factors_base_df$oil_return, na.rm = TRUE)


# ==============================================================================
# 2. EXPECTED SWAP VALUATION (Task 3a)
# ==============================================================================
print("Task 3a: Valuing Swap Payoffs...")

calc_swap_valuation <- function(z_array) {
  # Initialize tracking for the Expected PV and the raw payoffs (needed later for CVA)
  sim_pv_list <- numeric(n_sims)
  payoff_matrix <- matrix(0, nrow = n_sims, ncol = horizon_months)
  
  for(i in 1:n_sims) {
    sim_pv <- 0
    for(t in 1:horizon_months) {
      
      # Extract Z-scores for this specific month
      z_ai <- z_array[i, t, 1]
      z_oil <- z_array[i, t, 2]
      
      # Un-standardize the Z-scores to get the actual simulated market returns
      r_ai <- mu_ai + (z_ai * sd_ai)
      r_oil <- mu_oil + (z_oil * sd_oil)
      
      # Execute the payoff formula (Assuming it is defined in formulas.R)
      # If your function name differs, update the name here!
      payoff_t <- calc_hedge_payoff(
        notional = hedge_notional, 
        oil_ret = r_oil, 
        sw_ai_ret = r_ai, 
        fixed_spread = fixed_spread
      )
      
      # Store the raw monthly payoff for the CVA exposure calculation in 3b
      payoff_matrix[i, t] <- payoff_t
      
      # Discount the payoff to Present Value (t=0) and add to the simulation total
      sim_pv <- sim_pv + (payoff_t / (1 + risk_free/12)^t)
    }
    sim_pv_list[i] <- sim_pv
  }
  
  # Return both the average Expected Value and the full matrix for Task 3b
  return(list(
    Expected_PV = mean(sim_pv_list), 
    Payoff_Matrix = payoff_matrix
  ))
}

# Run 3a Valuations 
# (Make sure 'z_sim_bench' and 'z_sim_stress' match exactly what your Task 1 array outputs are named!)
swap_res_bench <- calc_swap_valuation(z_bench_array)
swap_res_stress <- calc_swap_valuation(z_stress_array)

# Print the final results for your report
cat(sprintf("-> Expected Swap Value (Benchmark): $%.2f\n", swap_res_bench$Expected_PV))
cat(sprintf("-> Expected Swap Value (Stress):    $%.2f\n", swap_res_stress$Expected_PV))
cat("------------------------------------------------------\n")

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

# Fix: Extract Payoff_Matrix from the newly created lists in Task 3a
cva_bench <- calc_cva(swap_res_bench$Payoff_Matrix)
cva_stress <- calc_cva(swap_res_stress$Payoff_Matrix)

cat(sprintf("Hedge CVA (Benchmark): $%.2f\n", cva_bench))
cat(sprintf("Hedge CVA (Stress):    $%.2f\n", cva_stress))


# ==============================================================================
# Task 3c: Net Hedge Value
# ==============================================================================
print("Task 3c: Calculating Net Hedge Value...")

# Fix: Extract Expected_PV from the newly created lists in Task 3a
net_bench <- swap_res_bench$Expected_PV - cva_bench
net_stress <- swap_res_stress$Expected_PV - cva_stress

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
# Fix: Updated to remove the single new function from 3a instead of the old names
rm(calc_swap_valuation, calc_cva)

# 4. Run garbage collection to free up memory from the large V_matrix inside the functions
gc()

cat("-> Environment cleaned. All final variables preserved. Ready for Task 4.\n")