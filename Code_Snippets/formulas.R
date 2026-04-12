# ==============================================================================
# File 2: formulas.R
# Contains all functional equations for simulating credit shocks, defaults, 
# and structural losses as specified in the assignment.
# ==============================================================================

# Calculate Sector-Credit Shock
# Page 3, Section 1C: A simple acceptable sector-credit shock [cite: 66, 68]
calc_sector_shock <- function(beta_ai, z_ai, beta_oil, z_oil, beta_rate, z_rate, S_jt) {
  shock <- -beta_ai * z_ai - beta_oil * z_oil + beta_rate * z_rate + S_jt
  return(shock)
}

# Calculate Current-Month Default Rate
# Page 3, Section 1C: A simple acceptable mapping from the sector credit shock [cite: 72, 73]
calc_default_rate <- function(p_j0, X_jt) {
  d_jt <- min(1, max(0, p_j0 * exp(X_jt)))
  return(d_jt)
}

# Calculate Cumulative Default Proportion
# Page 3, Section 1C: Cumulative default proportion tracking [cite: 76, 77]
calc_cumulative_default <- function(D_prev, d_jt) {
  D_jt <- D_prev + (1 - D_prev) * d_jt
  return(D_jt)
}

# Calculate Sector Valuation
# Page 3, Section 1D: A simple acceptable sector valuation at month t [cite: 84, 85, 86, 87]
calc_sector_valuation <- function(N_j, D_jt, c_j, M_t, r_t) {
  surviving_notional <- N_j * (1 - D_jt)
  
  # Safeguard: if maturity is reached (0 or less remaining months), return 0 value
  if (M_t <= 0) return(0)
  
  discount_rate <- r_t / 12
  coupon_pmt <- c_j / 12
  
  pv_factor <- sum(1 / (1 + discount_rate)^(1:M_t))
  pv_coupons <- coupon_pmt * pv_factor
  pv_principal <- 1 / (1 + discount_rate)^M_t
  
  V_jt <- surviving_notional * (pv_coupons + pv_principal)
  return(V_jt)
}

# Calculate Senior Tranche Cumulative Loss
# Page 4, Section 1E: Simple senior-tranche cumulative loss rate [cite: 95, 96]
calc_senior_tranche_loss <- function(pool_loss, attach_point, tranche_width) {
  # Relies on attach_point (0.20) and tranche_width (0.80) mapped from main.R
  senior_loss <- min(max(pool_loss - attach_point, 0), tranche_width) / tranche_width
  return(senior_loss)
}

# Calculate Forced-Sale Proceeds
# Page 4, Section 1E: Sale proceeds in liquidation event [cite: 101, 102]
calc_liquidation_proceeds <- function(rem_value, prop_spread, fire_sale_haircut) {
  # Relies on fire_sale_haircut (0.45) mapped from main.R
  proceeds <- rem_value * (1 - prop_spread) * (1 - fire_sale_haircut)
  return(proceeds)
}

# Calculate Monthly Hedge Payoff
# Page 4, Section 1F: The hedge pays to the fund monthly [cite: 110]
calc_hedge_payoff <- function(notional, oil_ret, sw_ai_ret, fixed_spread) {
  payoff <- notional * (oil_ret - sw_ai_ret + fixed_spread)
  return(payoff)
}