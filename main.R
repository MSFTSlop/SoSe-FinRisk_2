# install packages
install.packages("readxl")

# initialize packages
library(readxl)

# import given data
loan_pool_base_df <- read_excel("data/clo_pool_students.xlsx", sheet="Loan_Pool")
dealer_cds_curve_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Dealer_CDS_Curve")
historical_bid_ask_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Historical_Bid_Ask")
swap_parameters_base_df <- read_excel("data/hedge_counterparty_liquidity_students.xlsx", sheet="Swap_Parameters")
monthly_factors_base_df <- read_excel("data/market_factor_history_students.xlsx",sheet="Monthly_Factors")
monthly_levels_base_df <- read_excel("data/market_factor_history_students.xlsx",sheet="Monthly_Levels")
