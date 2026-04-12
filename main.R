# ==============================================================================
# File 1: main.R
# ==============================================================================
# Install packages if not already installed
# install.packages("readxl")

# Initialize packages
library(readxl)

# --- 1. DATA IMPORT ---
# Page 2, Section 1A: Provided Files [cite: 43, 44, 45, 46]
loan_pool_base_df <- read_excel("data/clo_pool_students.xlsx", sheet="Loan_Pool")
dealer_cds_curve_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Dealer_CDS_Curve")
historical_bid_ask_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Historical_Bid_Ask")
swap_parameters_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Swap_Parameters")
monthly_factors_base_df <- read_excel("data/market_factor_history_students.xlsx", sheet="Monthly_Factors")
monthly_levels_base_df <- read_excel("data/market_factor_history_students.xlsx", sheet="Monthly_Levels")

# --- 2. CONSTANTS & CASE PARAMETERS ---
# Horizon
horizon_months <- 60 # Page 2, Section 1A: Project the position over a horizon of 60 months [cite: 47]

# Position Parameters
senior_tranche_value <- 75000000 # Page 4, Section 1F: Trust is evaluating a $75 million position in the senior tranche [cite: 106]
market_price_pct <- 97 / 100 # Page 4, Section 1F: Quoted market price is 97.00 per 100 of par [cite: 107]
risk_free <- 0.02 # Page 4, Section 1F: Risk-free rate of 2% for discounting [cite: 111]

# CLO Tranche Structure Constraints
attach_point <- 0.20 # Page 5, Section 2B: Senior tranche attaches at 20% [cite: 128]
tranche_width <- 0.80 # Page 4, Section 1E: Senior tranche has width 80% [cite: 95]
trigger_threshold <- 0.08 # Page 4, Section 1E: 8% cumulative loss on the entire senior tranche [cite: 97]

# Liquidation & Counterparty Parameters
fire_sale_haircut <- 0.45 # Page 4, Section 1E: Fixed fire-sale haircut is 45% [cite: 103]
cva_recovery_rate <- 0.60 # Page 6, Question 3b: Recovery rate is 60% [cite: 167]

# --- 3. SOURCE FILES ---
# Source the formulas and calculation engines
source("Code_Snippets/formulas.R")

# Source the assignment tasks (comment these out to run individually)
# source("task1.R")
# source("task2.R")
# source("task3.R")
# source("task4.R")