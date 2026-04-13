# ==============================================================================
# File 6: task4.R
# Page 7, Question 4: Liquidity Risk and Final Recommendation (25%)
# ==============================================================================

# Note: Ensure main.R, formulas.R, task1.R, task2.R, and task3.R 
# environments are loaded.

print("Starting Task 4a: Estimating Liquidity Risk & Triggers...")

# Function to extract trigger statistics from the loss matrices
calc_trigger_stats <- function(senior_loss_matrix) {
  # trigger_threshold is imported from main.R (0.08)
  trigger_hit_matrix <- senior_loss_matrix > trigger_threshold
  
  # Identify which simulations hit the trigger at any point
  hit_any_vector <- apply(trigger_hit_matrix, 1, any)
  frequency <- mean(hit_any_vector)
  
  # For the simulations that DO hit the trigger, find the exact month
  if (sum(hit_any_vector) > 0) {
    trigger_months <- apply(trigger_hit_matrix[hit_any_vector, , drop = FALSE], 1, which.max)
    avg_month <- mean(trigger_months)
  } else {
    avg_month <- NA # Never hits trigger
  }
  
  return(list(Frequency = frequency, Avg_Month = avg_month))
}

stats_bench <- calc_trigger_stats(loss_res_bench$senior_loss)
stats_stress <- calc_trigger_stats(loss_res_stress$senior_loss)

# Print Task 4a Results
cat(sprintf("Benchmark Scenario - Trigger Hit: %.1f%% of the time | Average Month: %.1f\n", 
            stats_bench$Frequency * 100, stats_bench$Avg_Month))
cat(sprintf("Stress Scenario    - Trigger Hit: %.1f%% of the time | Average Month: %.1f\n", 
            stats_stress$Frequency * 100, stats_stress$Avg_Month))

cat(sprintf("Expected Forced-Sale Cost (Benchmark): $%.2f per $100 par\n", 
            risk_assessment_df$Liquidation_Penalty[risk_assessment_df$Scenario == "Benchmark"]))
cat(sprintf("Expected Forced-Sale Cost (Stress):    $%.2f per $100 par\n", 
            risk_assessment_df$Liquidation_Penalty[risk_assessment_df$Scenario == "Stress"]))


# ==============================================================================
# Task 4b: Recomputing Hedged Trust Value
# ==============================================================================
print("Task 4b: Recomputing Hedged Trust Value...")

# To combine the values, we need to convert the Net Hedge Value (which is in 
# absolute dollars based on a 5M notional) to the same scale as the CLO pricing 
# (which is priced per $100 of par based on a 75M notional).
# Scale = (Net Hedge Value / Senior Tranche Value) * 100

hedge_value_pct_bench <- (net_bench / senior_tranche_value) * 100
hedge_value_pct_stress <- (net_stress / senior_tranche_value) * 100

# Fetch Unhedged Values from Task 2's dataframe
unhedged_pct_bench <- risk_assessment_df$Unhedged_Liquidation_Value[risk_assessment_df$Scenario == "Benchmark"]
unhedged_pct_stress <- risk_assessment_df$Unhedged_Liquidation_Value[risk_assessment_df$Scenario == "Stress"]

# Calculate Final Hedged Value
hedged_pct_bench <- unhedged_pct_bench + hedge_value_pct_bench
hedged_pct_stress <- unhedged_pct_stress + hedge_value_pct_stress


# ==============================================================================
# Task 4c: Final Recommendation Analysis
# ==============================================================================
print("Task 4c & 4d: Final Recommendation Matrix...")

# Build the final comprehensive dataframe
final_decision_df <- data.frame(
  Scenario = c("Benchmark", "Stress"),
  Market_Price = c(market_price_pct * 100, market_price_pct * 100),
  Unhedged_Value = c(unhedged_pct_bench, unhedged_pct_stress),
  Buy_Unhedged = c(unhedged_pct_bench > (market_price_pct * 100), unhedged_pct_stress > (market_price_pct * 100)),
  Net_Hedge_Value_Pct = round(c(hedge_value_pct_bench, hedge_value_pct_stress), 2),
  Hedged_Total_Value = c(hedged_pct_bench, hedged_pct_stress),
  Buy_Hedged = c(hedged_pct_bench > (market_price_pct * 100), hedged_pct_stress > (market_price_pct * 100))
)

print(final_decision_df)

# Output summary strings for the report
cat("\n--- FINAL DECISION SUMMARY ---\n")
cat("If Unhedged (Benchmark):", ifelse(final_decision_df$Buy_Unhedged[1], "BUY", "DO NOT BUY"), "\n")
cat("If Unhedged (Stress):   ", ifelse(final_decision_df$Buy_Unhedged[2], "BUY", "DO NOT BUY"), "\n")
cat("If Hedged (Benchmark):  ", ifelse(final_decision_df$Buy_Hedged[1], "BUY", "DO NOT BUY"), "\n")
cat("If Hedged (Stress):     ", ifelse(final_decision_df$Buy_Hedged[2], "BUY", "DO NOT BUY"), "\n")

# ==============================================================================
# 5. ENVIRONMENT CLEANUP (End of Task 4)
# ==============================================================================
print("Task 4 Complete. Cleaning up final temporary variables...")

# 1. Remove intermediate trigger stats lists
rm(stats_bench, stats_stress)

# 2. Remove intermediate percentage conversion variables
rm(hedge_value_pct_bench, hedge_value_pct_stress, 
   unhedged_pct_bench, unhedged_pct_stress, 
   hedged_pct_bench, hedged_pct_stress)

# 3. Remove the Task 4 helper function
rm(calc_trigger_stats)

# 4. Final garbage collection
gc()

cat("-> Environment cleaned. The 'final_decision_df' is saved and the project is complete!\n")