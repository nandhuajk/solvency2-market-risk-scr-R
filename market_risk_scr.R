# ==============================================================================
# Solvency II Market Risk SCR Calculator & ALM Dashboard (Standard Formula)
# ==============================================================================

library(ggplot2)
library(patchwork)

# 1. Equity Risk SCR Function
calculate_equity_scr <- function(portfolio, symmetric_adjustment = 0.02) {
  type1_val <- sum(portfolio$Market_Value[portfolio$Asset_Class == "Equity_Type1"], na.rm = TRUE)
  type2_val <- sum(portfolio$Market_Value[portfolio$Asset_Class == "Equity_Type2"], na.rm = TRUE)
  
  # EIOPA base shocks: 39% for Type 1 (OECD/EEA), 49% for Type 2 (Other)
  scr_type1 <- type1_val * (0.39 + symmetric_adjustment)
  scr_type2 <- type2_val * (0.49 + symmetric_adjustment)
  
  # Correlation between Type 1 and Type 2 equities = 0.75
  corr_equity <- 0.75
  scr_equity <- sqrt(scr_type1^2 + scr_type2^2 + 2 * corr_equity * scr_type1 * scr_type2)
  
  return(scr_equity)
}

# 2. Interest Rate Risk SCR Function (Asset-Liability Management)
calculate_interest_rate_scr <- function(portfolio, liabilities_pv, liabilities_duration) {
  shock_up <- 0.01     # +100 bps parallel shift proxy
  shock_down <- -0.01  # -100 bps parallel shift proxy
  
  bonds <- subset(portfolio, Asset_Class == "Corporate_Bond")
  
  # Change in Bond Values: Delta V = -Duration * V * Delta y
  delta_assets_up <- -sum(bonds$Duration * bonds$Market_Value * shock_up, na.rm = TRUE)
  delta_assets_down <- -sum(bonds$Duration * bonds$Market_Value * shock_down, na.rm = TRUE)
  
  # Change in Technical Provisions (Liabilities)
  delta_liab_up <- -liabilities_duration * liabilities_pv * shock_up
  delta_liab_down <- -liabilities_duration * liabilities_pv * shock_down
  
  # Net Asset Value (NAV) Impact = Delta Assets - Delta Liabilities
  nav_change_up <- delta_assets_up - delta_liab_up
  nav_change_down <- delta_assets_down - delta_liab_down
  
  # SCR is the worst capital loss
  scr_up <- max(0, -nav_change_up)
  scr_down <- max(0, -nav_change_down)
  
  if (scr_up >= scr_down) {
    return(list(scr = scr_up, scenario = "Rates Up"))
  } else {
    return(list(scr = scr_down, scenario = "Rates Down"))
  }
}

# 3. Credit Spread Risk SCR Function
calculate_spread_scr <- function(portfolio) {
  spread_lookup <- c("AAA" = 0.009, "AA" = 0.011, "A" = 0.014, "BBB" = 0.025)
  
  bonds <- subset(portfolio, Asset_Class == "Corporate_Bond")
  bonds$Stress_Factor <- spread_lookup[bonds$Rating]
  bonds$Spread_Loss <- bonds$Market_Value * bonds$Duration * bonds$Stress_Factor
  
  return(sum(bonds$Spread_Loss, na.rm = TRUE))
}

# 4. Main Aggregation Function
aggregate_market_scr <- function(portfolio, liabilities_pv, liabilities_duration) {
  ir_res <- calculate_interest_rate_scr(portfolio, liabilities_pv, liabilities_duration)
  scr_ir <- ir_res$scr
  biting_scenario <- ir_res$scenario
  
  scr_eq <- calculate_equity_scr(portfolio)
  scr_sp <- calculate_spread_scr(portfolio)
  
  # Dynamic correlation parameter A (0 if Rates Up bites, 0.5 if Rates Down bites)
  corr_ir_other <- ifelse(biting_scenario == "Rates Up", 0.0, 0.5)
  corr_eq_sp <- 0.75
  
  scr_vector <- c(scr_ir, scr_eq, scr_sp)
  
  # 3x3 EIOPA Correlation Matrix
  corr_matrix <- matrix(c(
    1.0,           corr_ir_other, corr_ir_other,
    corr_ir_other, 1.0,           corr_eq_sp,
    corr_ir_other, corr_eq_sp,    1.0
  ), nrow = 3, byrow = TRUE)
  
  total_market_scr <- as.numeric(sqrt(t(scr_vector) %*% corr_matrix %*% scr_vector))
  undiversified_scr <- sum(scr_vector)
  diversification_benefit <- undiversified_scr - total_market_scr
  
  results <- data.frame(
    Metric = c(
      "Biting IR Scenario",
      "SCR Interest Rate (€M)",
      "SCR Equity (€M)",
      "SCR Spread (€M)",
      "Undiversified Sum (€M)",
      "Diversification Benefit (€M)",
      "Total Market Risk SCR (€M)"
    ),
    Value = c(
      biting_scenario,
      round(scr_ir, 2),
      round(scr_eq, 2),
      round(scr_sp, 2),
      round(undiversified_scr, 2),
      round(diversification_benefit, 2),
      round(total_market_scr, 2)
    )
  )
  return(results)
}

# ==============================================================================
# 5. Execute Model & Build Multi-Panel Executive Dashboard
# ==============================================================================

sample_assets <- data.frame(
  Asset_Name = c("Eurozone Gov/Corp AAA", "EU Corporate A", "EU Corporate BBB", "CAC 40 ETF", "Private Equity Fund"),
  Asset_Class = c("Corporate_Bond", "Corporate_Bond", "Corporate_Bond", "Equity_Type1", "Equity_Type2"),
  Market_Value = c(300.0, 250.0, 150.0, 200.0, 100.0),
  Duration = c(7.5, 5.2, 4.0, 0.0, 0.0),
  Rating = c("AAA", "A", "BBB", NA, NA),
  stringsAsFactors = FALSE
)

best_estimate_liabilities <- 850.0
liability_duration <- 8.0

report <- aggregate_market_scr(sample_assets, best_estimate_liabilities, liability_duration)
print(report, row.names = FALSE)

# --- Panel 1: Portfolio Asset Allocation (Donut Chart) ---
alloc_df <- aggregate(Market_Value ~ Asset_Class, data = sample_assets, sum)
alloc_df$Pct <- round(alloc_df$Market_Value / sum(alloc_df$Market_Value) * 100, 1)

p1 <- ggplot(alloc_df, aes(x = 2, y = Market_Value, fill = Asset_Class)) +
  geom_col(color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = paste0(Pct, "%")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("#00008F", "#4B6CB7", "#FF1721")) +
  labs(title = "1. Portfolio Allocation (€1,000M)", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5), legend.position = "bottom")

# --- Panel 2: ALM Interest Rate Sensitivity Curve ---
shocks <- seq(-0.02, 0.02, by = 0.0025)
bonds <- subset(sample_assets, Asset_Class == "Corporate_Bond")
bond_total <- sum(bonds$Market_Value)
bond_dur <- sum(bonds$Market_Value * bonds$Duration) / bond_total

nav_curve <- sapply(shocks, function(s) {
  d_assets <- -bond_dur * bond_total * s
  d_liab   <- -liability_duration * best_estimate_liabilities * s
  (1000 + d_assets) - (best_estimate_liabilities + d_liab)
})

alm_df <- data.frame(Shock_Bps = shocks * 10000, NAV = nav_curve)

p2 <- ggplot(alm_df, aes(x = Shock_Bps, y = NAV)) +
  geom_line(color = "#00008F", linewidth = 1.2) +
  geom_hline(yintercept = 150, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -100, linetype = "dotted", color = "#FF1721", linewidth = 1) +
  annotate("text", x = -95, y = 175, label = "Solvency II\n-100 bps Shock", color = "#FF1721", hjust = 0, size = 3, fontface = "bold") +
  labs(title = "2. ALM Sensitivity: NAV vs. Rate Shock", x = "Interest Rate Shock (bps)", y = "Net Asset Value (€M)") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

# --- Panel 3: SCR Waterfall Breakdown ---
plot_data <- data.frame(
  Component = factor(
    c("Interest Rate SCR", "Equity SCR", "Credit Spread SCR", "Diversification Benefit", "Total Net Market SCR"),
    levels = c("Interest Rate SCR", "Equity SCR", "Credit Spread SCR", "Diversification Benefit", "Total Net Market SCR")
  ),
  Amount = as.numeric(c(
    report$Value[report$Metric == "SCR Interest Rate (€M)"],
    report$Value[report$Metric == "SCR Equity (€M)"],
    report$Value[report$Metric == "SCR Spread (€M)"],
    -as.numeric(report$Value[report$Metric == "Diversification Benefit (€M)"]),
    report$Value[report$Metric == "Total Market Risk SCR (€M)"]
  )),
  Type = c("Sub-Module", "Sub-Module", "Sub-Module", "Diversification", "Net Total")
)

p3 <- ggplot(plot_data, aes(x = Component, y = Amount, fill = Type)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(aes(label = paste0("€", round(Amount, 1), "M")), vjust = ifelse(plot_data$Amount >= 0, -0.4, 1.3), fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c("Sub-Module" = "#00008F", "Diversification" = "#2E8B57", "Net Total" = "#FF1721")) +
  scale_y_continuous(limits = c(-60, 210)) +
  labs(title = "3. Solvency II Market Risk SCR & Diversification Relief", x = NULL, y = "Capital (€M)") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 11))

# Combine and save
dashboard <- (p1 | p2) / p3 +
  plot_annotation(
    title = "Solvency II Market Risk & ALM Dashboard",
    subtitle = "Standard Formula Capital Charge & Duration Gap Sensitivity",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(dashboard)
ggsave("solvency2_dashboard.png", plot = dashboard, width = 10, height = 7, dpi = 300)
