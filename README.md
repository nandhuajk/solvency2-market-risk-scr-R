# Solvency II Market Risk SCR Calculator (R)

An implementation of the **Solvency II Pillar 1 Market Risk Solvency Capital Requirement (SCR)** under the EIOPA Standard Formula in **R**.

![Solvency II Dashboard](solvency2_dashboard.png)

## Features
- **Interest Rate Risk (ALM):** Evaluates +100 bps and -100 bps parallel yield curve shocks across bonds and long-duration liabilities.
- **Equity Risk:** Applies EIOPA Type 1 (39%) and Type 2 (49%) shocks with a 0.75 correlation factor.
- **Credit Spread Risk:** Computes duration-weighted spread shocks across AAA to BBB ratings.
- **Dynamic Correlation Matrix:** Switches the interest rate correlation parameter (`0.0` vs. `0.5`) depending on the biting rate scenario.
