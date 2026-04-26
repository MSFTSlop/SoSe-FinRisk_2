# SoSe-FinRisk
Fin Risk Project 2

## Disclaimer

This code was heavily vibecoded. Be advised that the author may not have grasped
the full calculation of the used concepts. Instead he had too much fun overengineering
a simple assignment XD

## Project Architecture
This project models the collateral losses, senior tranche valuation, liquidity risk, and macro-hedging of a Private-Credit CLO over a 60-month horizon.

To keep things clean, the project is strictly divided into specialized files:

- main.R: The central hub. Imports all data, sets global constants (horizon, pricing, haircuts), and loads libraries. Run this first.

- formulas.R: The math engine. Contains all the raw financial equations (hazard rates, sector shocks, liquidation proceeds).

- task1.R to task4.R: The operational scripts that execute the actual assignment requirements sequentially.

## Deep Dive: What happens in task1.R?
Task 1 is the scenario engine for the entire project. Since markets behave differently in normal times versus crisis times, this script builds two distinct parallel universes (a Benchmark scenario and a Stress scenario) for our simulated risk factors (AI Software, Oil, and Interest Rates).

Here is exactly how it does it:

### The Manual Windowing Strategy
Instead of using the entire 25-year history of the market, the script chunks the historical data into roughly 5-year "windows". It then runs statistical tests (Excess Kurtosis, Ljung-Box tests for autocorrelation) on each window to find where the data looks the most chaotic or "fat-tailed."

- Manual Selection: Based on these diagnostics, we manually hardcoded the selection:

- Benchmark Scenario: Mapped to Window 3 (2011-2015)

- Stress Scenario: Mapped to Window 2 (2006-2010)

### The Simulation (Copulas & Cholesky)
Once the windows are selected, the script extracts their specific correlation matrices and simulates 1,000 parallel futures (over 60 months) using Cholesky Decomposition.

We check if the excess curtosis exceeds 1.0 (rule of thumb since perfect normality doesnt exist
in financial markets) and the p-value is smaller 0.05 (95% confidence) for the ljung-box
test to check for autocorrellation.

Depending on the excess curtosis you can manually set it up to use either:
- Linear Gaussian 
- student - t

Currently its manually set to Student-T

### Visualization
In the folder "visualization" you can see the factor distributions and copula tail dependence to help in the analysis

## Deep Dive: What happens in task2.R?
Task 2 we dive into the collateral losses and seniro tranche valuation

### General things to know
Line 25: given the provided coupons and weights it is standard practice
to use a WAC-Formula

Line 46: this is the so called Idiosyncratic risk, generated on the fly by R
for every month and every sector a new independendt standard normal random number
is being generated

the Z-Arrays: Task 1 we calculated the Z-Scores for AI, Oil and the rates
these arrays are a massive 3D construct
1000 Simulations x 60 months x 3 Variables (1=AI; 2=Oil; 3=Rate)

### The Distress Free Investor
Since the 75 mil tranche is being evaluated as a whole, the pricing 
logic treats it as a bond (thats why in line 25 the WAC-Formula exists).

At the end we subtract the losses which chewed through the 20% buffer.

