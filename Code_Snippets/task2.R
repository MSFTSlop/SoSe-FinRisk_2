# ==============================================================================
# File 4: task2.R
# Page 6, Question 2: Collateral Losses and Senior Tranche Valuation (35%)
# ==============================================================================

# Note: Ensure main.R and formulas.R are sourced before running this
# source("main.R")
# source("formulas.R")

print("Starting Task 2a: Simulating Pool and Tranche Losses...")

# 1. EXTRACT POOL PARAMETERS (Imported from main.R environment)
# ==============================================================================
n_sectors <- nrow(loan_pool_base_df)
pool_weights <- loan_pool_base_df$Pool_Weight
base_pds <- loan_pool_base_df$Base_PD_Monthly
base_recoveries <- loan_pool_base_df$Base_Recovery

beta_ai <- loan_pool_base_df$AI_beta
beta_oil <- loan_pool_base_df$Oil_beta
beta_rate <- loan_pool_base_df$Rate_beta

# Calculate the weighted average coupon of the underlying pool 
# (Used as the baseline coupon yield for the senior tranche)
wa_coupon <- sum(loan_pool_base_df$Coupon * pool_weights)


# 2. CORE SIMULATION ENGINE (Task 2a)
# ==============================================================================
run_loss_simulation <- function(z_array, n_sims, horizon_months) {
  
  # Initialize tracking matrices
  pool_loss_matrix <- matrix(0, nrow = n_sims, ncol = horizon_months)
  senior_loss_matrix <- matrix(0, nrow = n_sims, ncol = horizon_months)
  
  # MATHEMATICAL DECISION 1: EXACT SECTOR DEFAULT TRACKING
  # We track the exact cumulative default of every individual sector to preserve 
  # dispersion during a fire-sale, rather than using a blended pool average.
  sector_default_array <- array(0, dim = c(n_sims, horizon_months, n_sectors))
  
  for (i in 1:n_sims) {
    cum_defaults <- rep(0, n_sectors) 
    
    for (t in 1:horizon_months) {
      # Draw Idiosyncratic Risk
      S_jt <- rnorm(n_sectors, mean = 0, sd = 1)
      
      # Extract Z-scores for month t
      z_ai_t   <- z_array[i, t, 1]
      z_oil_t  <- z_array[i, t, 2]
      z_rate_t <- z_array[i, t, 3]
      
      current_pool_loss <- 0
      
      for (j in 1:n_sectors) {
        # Relying strictly on formulas.R for mathematical logic
        X_jt <- calc_sector_shock(beta_ai[j], z_ai_t, beta_oil[j], z_oil_t, beta_rate[j], z_rate_t, S_jt[j])
        d_jt <- calc_default_rate(base_pds[j], X_jt)
        cum_defaults[j] <- calc_cumulative_default(cum_defaults[j], d_jt)
        
        # Save exact sector default for Task 2c liquidation valuation
        sector_default_array[i, t, j] <- cum_defaults[j]
        
        # Sector Loss = Cumulative Defaults * Loss Given Default (1 - Recovery) 
        ## basically the lines 67, 68 and 72 are in sum the LPool Loss function
        ## from the assignment
        sector_loss <- cum_defaults[j] * (1 - base_recoveries[j])
        current_pool_loss <- current_pool_loss + (sector_loss * pool_weights[j])
      }
      
      # Store total pool loss
      pool_loss_matrix[i, t] <- current_pool_loss
      
      # Map to Senior Tranche using formulas.R (relies on main.R parameters)
      senior_loss_matrix[i, t] <- calc_senior_tranche_loss(current_pool_loss, attach_point, tranche_width)
    }
  }
  
  return(list(
    pool_loss = pool_loss_matrix, 
    senior_loss = senior_loss_matrix,
    sector_defaults = sector_default_array
  ))
}

# Run the simulations
n_sims <- dim(z_bench_array)[1]
print("Running Benchmark Simulation...")
loss_res_bench <- run_loss_simulation(z_bench_array, n_sims, horizon_months)

print("Running Stress Simulation...")
loss_res_stress <- run_loss_simulation(z_stress_array, n_sims, horizon_months)

# ==============================================================================
# REPORTING TASK 2A: EXPECTED LOSS, VaR, and ES AT MONTH 60
# ==============================================================================
print("Reporting Task 2a Results (All 6 Metrics)...")

# Define Confidence Level for VaR/ES (99%)
alpha <- 0.99

# --- 1. POOL LOSS METRICS (Month 60) ---
pool_60_bench <- loss_res_bench$pool_loss[, horizon_months]
pool_60_stress <- loss_res_stress$pool_loss[, horizon_months]

# Benchmark
el_pool_bench <- mean(pool_60_bench)
var_pool_bench <- quantile(pool_60_bench, alpha)
es_pool_bench <- mean(pool_60_bench[pool_60_bench >= var_pool_bench])

# Stress
el_pool_stress <- mean(pool_60_stress)
var_pool_stress <- quantile(pool_60_stress, alpha)
es_pool_stress <- mean(pool_60_stress[pool_60_stress >= var_pool_stress])

# --- 2. SENIOR TRANCHE LOSS METRICS (Month 60) ---
senior_60_bench <- loss_res_bench$senior_loss[, horizon_months]
senior_60_stress <- loss_res_stress$senior_loss[, horizon_months]

# Benchmark
el_senior_bench <- mean(senior_60_bench)
var_senior_bench <- quantile(senior_60_bench, alpha)
es_senior_bench <- mean(senior_60_bench[senior_60_bench >= var_senior_bench])

# Stress
el_senior_stress <- mean(senior_60_stress)
var_senior_stress <- quantile(senior_60_stress, alpha)
es_senior_stress <- mean(senior_60_stress[senior_60_stress >= var_senior_stress])

# --- 3. CREATE CLEAN REPORTING DATAFRAME ---
task2a_results <- data.frame(
  Metric = c("Expected Loss (EL)", "Value at Risk (VaR 99%)", "Expected Shortfall (ES 99%)"),
  Pool_Benchmark_Pct = c(el_pool_bench, var_pool_bench, es_pool_bench) * 100,
  Pool_Stress_Pct = c(el_pool_stress, var_pool_stress, es_pool_stress) * 100,
  Senior_Benchmark_Pct = c(el_senior_bench, var_senior_bench, es_senior_bench) * 100,
  Senior_Stress_Pct = c(el_senior_stress, var_senior_stress, es_senior_stress) * 100
)

# Print out the results table for the report
print(task2a_results)
cat("------------------------------------------------------\n")


# 3. DISTRESS-FREE VALUATION (Task 2b)
# ==============================================================================
print("Starting Task 2b: Distress-Free Valuation...")

calc_distress_free_value <- function(senior_loss_matrix) {
  pv_list <- numeric(n_sims)
  
  for(i in 1:n_sims) {
    sim_pv <- 0
    for(t in 1:horizon_months) {
      surviving_senior_fraction <- 1 - senior_loss_matrix[i, t]
      monthly_coupon_cf <- (wa_coupon / 12) * surviving_senior_fraction
      sim_pv <- sim_pv + (monthly_coupon_cf / (1 + risk_free/12)^t)
      
      # Principal repayment at maturity
      if(t == horizon_months) {
        sim_pv <- sim_pv + (surviving_senior_fraction / (1 + risk_free/12)^t)
      }
    }
    pv_list[i] <- sim_pv * 100 # Scale to price per $100 par
  }
  return(mean(pv_list))
}

val_df_bench <- calc_distress_free_value(loss_res_bench$senior_loss)
val_df_stress <- calc_distress_free_value(loss_res_stress$senior_loss)

cat(sprintf("Distress-Free Value (Benchmark): $%.2f per $100 par\n", val_df_bench))
cat(sprintf("Distress-Free Value (Stress):    $%.2f per $100 par\n", val_df_stress))

# ==============================================================================
# 4. UNHEDGED BUY-SIDE VALUATION [LIQUIDATION RISK] (Task 2c)
# ==============================================================================
print("Starting Task 2c: Unhedged Buy-Side Valuation...")

# Dynamically calculate the proportional spread using the correct decimal column
avg_bid_ask_spread <- mean(historical_bid_ask_base_df$bid_ask_spread_decimal, na.rm = TRUE)

calc_unhedged_liquidation_value <- function(senior_loss_matrix, sector_default_array) {
  pv_list <- numeric(n_sims)
  trigger_count <- 0
  
  for(i in 1:n_sims) {
    # trigger_threshold imported from main.R
    trigger_hit <- senior_loss_matrix[i, ] > trigger_threshold 
    
    if(any(trigger_hit)) {
      trigger_count <- trigger_count + 1
      T_trigger <- which.max(trigger_hit) # Month of the panic fire-sale
      
      sim_pv <- 0
      
      # Cash flows BEFORE the trigger
      if(T_trigger > 1) {
        for(t in 1:(T_trigger - 1)) {
          surviving_senior <- 1 - senior_loss_matrix[i, t]
          sim_pv <- sim_pv + (((wa_coupon / 12) * surviving_senior) / (1 + risk_free/12)^t)
        }
      }
      
      # Value the surviving pool AT the trigger month using formulas.R
      pool_val_at_trigger <- 0
      for(j in 1:n_sectors) {
        exact_D_jt <- sector_default_array[i, T_trigger, j]
        
        sector_val <- calc_sector_valuation(
          N_j = pool_weights[j], 
          D_jt = exact_D_jt, 
          c_j = loan_pool_base_df$Coupon[j], 
          M_t = horizon_months - T_trigger, 
          r_t = risk_free
        )
        pool_val_at_trigger <- pool_val_at_trigger + sector_val
      }
      
      # MODULARITY FIX: Call the exact liquidation formula from formulas.R
      # Passes the remaining pool value, proportional spread, and fire sale haircut
      proceeds_after_haircut <- calc_liquidation_proceeds(
        rem_value = pool_val_at_trigger, 
        prop_spread = avg_bid_ask_spread, 
        fire_sale_haircut = fire_sale_haircut
      )
      
      # MATHEMATICAL DECISION: SENIOR CLAIM WATERFALL SCALING
      # Based on CLO structure in the assignment, the Senior Tranche is only 80% of the pool (tranche_width),
      # but it has FIRST CLAIM on 100% of the liquidation cash. We scale the proceeds up by dividing by 
      # the width, capped at the maximum surviving principal they are owed.
      surviving_senior_at_trigger <- 1 - senior_loss_matrix[i, T_trigger]
      senior_claim <- min(surviving_senior_at_trigger, proceeds_after_haircut / tranche_width) 
      
      # Discount the liquidation payout to t=0
      sim_pv <- sim_pv + (senior_claim / (1 + risk_free/12)^T_trigger)
      pv_list[i] <- sim_pv * 100
      
    } else {
      # No trigger hit -> Survives to maturity
      sim_pv <- 0
      for(t in 1:horizon_months) {
        surviving_senior <- 1 - senior_loss_matrix[i, t]
        sim_pv <- sim_pv + (((wa_coupon / 12) * surviving_senior) / (1 + risk_free/12)^t)
      }
      sim_pv <- sim_pv + ((1 - senior_loss_matrix[i, horizon_months]) / (1 + risk_free/12)^horizon_months)
      pv_list[i] <- sim_pv * 100
    }
  }
  
  return(list(Expected_Value = mean(pv_list), Trigger_Prob = trigger_count / n_sims))
}

# Run 2c Valuations
val_liq_bench <- calc_unhedged_liquidation_value(loss_res_bench$senior_loss, loss_res_bench$sector_defaults)
val_liq_stress <- calc_unhedged_liquidation_value(loss_res_stress$senior_loss, loss_res_stress$sector_defaults)

cat(sprintf("Unhedged Value (Benchmark): $%.2f per $100 par (Trigger Prob: %.1f%%)\n", val_liq_bench$Expected_Value, val_liq_bench$Trigger_Prob * 100))
cat(sprintf("Unhedged Value (Stress):    $%.2f per $100 par (Trigger Prob: %.1f%%)\n", val_liq_stress$Expected_Value, val_liq_stress$Trigger_Prob * 100))

# 5. RISK ASSESSMENT DATAFRAME (Task 2d)
# ==============================================================================
print("Generating 2D Summary for Risk Assessment (Task 2d)...")

risk_assessment_df <- data.frame(
  Scenario = c("Benchmark", "Stress"),
  Distress_Free_Value = round(c(val_df_bench, val_df_stress), 2),
  Unhedged_Liquidation_Value = round(c(val_liq_bench$Expected_Value, val_liq_stress$Expected_Value), 2),
  Liquidation_Penalty = round(c(val_df_bench - val_liq_bench$Expected_Value, val_df_stress - val_liq_stress$Expected_Value), 2),
  Market_Price = round(c(market_price_pct * 100, market_price_pct * 100), 2)
)

risk_assessment_df$Valid_Buy <- risk_assessment_df$Unhedged_Liquidation_Value > risk_assessment_df$Market_Price

print(risk_assessment_df)

# ==============================================================================
# 6. ENVIRONMENT CLEANUP (End of Task 2)
# ==============================================================================
print("Task 2 Complete. Cleaning up temporary environment variables...")

# Remove temporary pool parameters and extraction vectors
rm(n_sectors, pool_weights, base_pds, base_recoveries, 
   beta_ai, beta_oil, beta_rate, wa_coupon, avg_bid_ask_spread)

# Remove the Task 2 specific functions (outputs are safely stored)
rm(run_loss_simulation, calc_unhedged_liquidation_value)