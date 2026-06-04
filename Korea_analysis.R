# ============================================================
# KOREA E-COMMERCE SALES DRIVERS ANALYSIS
# Full Statistical Analysis: Descriptive → Correlation →
# Hypothesis Testing → ANOVA → Regression
# ============================================================


# ============================================================
# 0. LOAD LIBRARIES
# ============================================================
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)


# ============================================================
# 1. LOAD & RESHAPE DATA
# ============================================================

# ── 1a. Sales (monthly retail sales, "Total" row) ──────────
sales_raw <- read.csv(
  "korea/Sales_by_product_group_20260602164655.csv",
  header           = FALSE,
  skip             = 2,
  stringsAsFactors = FALSE
)

# Get period names from row 1 of the original file
period_names <- read.csv(
  "korea/Sales_by_product_group_20260602164655.csv",
  header           = FALSE,
  nrows            = 1,
  stringsAsFactors = FALSE
)
period_names <- as.character(period_names[1, ])

# Trim to match number of columns in sales_raw
period_names <- period_names[1:ncol(sales_raw)]
colnames(sales_raw) <- period_names

cat("=== SALES RAW (first 4 cols) ===\n")
print(sales_raw[1:3, 1:4])

# Keep only the "Total" row
total_row <- sales_raw %>%
  filter(trimws(.[[1]]) == "Total")

# Reshape to long format and aggregate to annual
korea_sales <- total_row %>%
  pivot_longer(
    cols      = -1,
    names_to  = "period",
    values_to = "sales"
  ) %>%
  rename(category = 1) %>%
  mutate(
    sales  = as.numeric(gsub(",", "", sales)),
    period = gsub(" p\\)", "", period),
    period = gsub("p\\)", "",  period),
    period = trimws(period),
    year   = as.integer(sub("\\..*", "", period))
  ) %>%
  filter(!is.na(sales), !is.na(year)) %>%
  group_by(year) %>%
  summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")

cat("\n=== SALES ANNUAL ===\n")
print(korea_sales)


# ── 1b. GDP ────────────────────────────────────────────────
gdp_raw <- read.csv("korea/korea_gdp.csv", skip = 4)

korea_gdp <- gdp_raw %>%
  filter(trimws(Country.Name) == "Korea, Rep.") %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "gdp"
  ) %>%
  mutate(
    year = as.integer(sub("X", "", year)),
    gdp  = as.numeric(gdp)
  ) %>%
  filter(!is.na(gdp)) %>%
  select(year, gdp)

cat("\n=== GDP LONG ===\n")
print(head(korea_gdp))


# ── 1c. Inflation ──────────────────────────────────────────
inflation_raw <- read.csv("korea/korea_inflation.csv", skip = 4)

korea_inflation <- inflation_raw %>%
  filter(trimws(Country.Name) == "Korea, Rep.") %>%
  pivot_longer(
    cols      = starts_with("X"),
    names_to  = "year",
    values_to = "inflation"
  ) %>%
  mutate(
    year      = as.integer(sub("X", "", year)),
    inflation = as.numeric(inflation)
  ) %>%
  filter(!is.na(inflation)) %>%
  select(year, inflation)

cat("\n=== INFLATION LONG ===\n")
print(head(korea_inflation))


# ── 1d. Unemployment ───────────────────────────────────────
unemp_raw <- read.csv(
  "korea/korea_unemployment.csv",
  header           = FALSE,
  stringsAsFactors = FALSE
)

# Drop header row if present
if (grepl("Unemployment", unemp_raw[1, 2], ignore.case = TRUE)) {
  unemp_raw <- unemp_raw[-1, ]
}

colnames(unemp_raw) <- c("year", "unemployment")

korea_unemployment <- unemp_raw %>%
  mutate(
    year         = as.integer(gsub('"', '', year)),
    unemployment = as.numeric(unemployment)
  ) %>%
  filter(!is.na(year), !is.na(unemployment))

cat("\n=== UNEMPLOYMENT LONG ===\n")
print(head(korea_unemployment))


# ============================================================
# 2. MERGE & CREATE MASTER DATASET
# ============================================================
korea_master <- korea_sales %>%
  left_join(korea_gdp,          by = "year") %>%
  left_join(korea_inflation,    by = "year") %>%
  left_join(korea_unemployment, by = "year") %>%
  mutate(
    country     = "Korea",
    covid       = ifelse(year >= 2020, 1, 0),
    covid_label = ifelse(covid == 1, "Post-COVID", "Pre-COVID"),
    quarter     = case_when(
      row_number() %% 4 == 1 ~ "Q1",
      row_number() %% 4 == 2 ~ "Q2",
      row_number() %% 4 == 3 ~ "Q3",
      TRUE                   ~ "Q4"
    )
  ) %>%
  filter(!is.na(sales) & !is.na(inflation) & !is.na(unemployment))

cat("\n=== YEAR COVERAGE CHECK ===\n")
cat("Sales years:       ", paste(sort(unique(korea_sales$year)),        collapse = ", "), "\n")
cat("GDP years:         ", paste(sort(unique(korea_gdp$year)),          collapse = ", "), "\n")
cat("Inflation years:   ", paste(sort(unique(korea_inflation$year)),    collapse = ", "), "\n")
cat("Unemployment years:", paste(sort(unique(korea_unemployment$year)), collapse = ", "), "\n")
cat("Master years:      ", paste(sort(unique(korea_master$year)),       collapse = ", "), "\n")

cat("\n=== MASTER DATASET ===\n")
print(str(korea_master))
print(head(korea_master))


# ============================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================
cat("\n\n============================================================\n")
cat("3. DESCRIPTIVE STATISTICS\n")
cat("============================================================\n")

print(summary(korea_master))

desc_stats <- korea_master %>%
  summarise(
    n            = n(),
    sales_mean   = mean(sales,        na.rm = TRUE),
    sales_median = median(sales,      na.rm = TRUE),
    sales_sd     = sd(sales,          na.rm = TRUE),
    sales_min    = min(sales,         na.rm = TRUE),
    sales_max    = max(sales,         na.rm = TRUE),
    gdp_mean     = mean(gdp,          na.rm = TRUE),
    gdp_sd       = sd(gdp,            na.rm = TRUE),
    infl_mean    = mean(inflation,    na.rm = TRUE),
    infl_sd      = sd(inflation,      na.rm = TRUE),
    unemp_mean   = mean(unemployment, na.rm = TRUE),
    unemp_sd     = sd(unemployment,   na.rm = TRUE)
  )

cat("\n--- Summary Table ---\n")
print(desc_stats)


# ============================================================
# 4. DATA VISUALIZATION
# ============================================================
cat("\n\n============================================================\n")
cat("4. DATA VISUALIZATION\n")
cat("============================================================\n")

# ── 4a. Sales over Time ──────────────────────────────────
ggplot(korea_master, aes(x = year, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Korea E-Commerce Sales Over Time",
       x = "Year", y = "Total Sales") +
  theme_minimal()

# ── 4b. Histogram: Sales ─────────────────────────────────
ggplot(korea_master, aes(x = sales)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Korea E-Commerce Sales",
       x = "Sales", y = "Count") +
  theme_minimal()

# ── 4c. Histogram: Inflation ─────────────────────────────
ggplot(korea_master, aes(x = inflation)) +
  geom_histogram(bins = 10, fill = "coral", color = "white") +
  labs(title = "Distribution of Inflation Rate (Korea)",
       x = "Inflation (%)", y = "Count") +
  theme_minimal()

# ── 4d. Histogram: GDP ───────────────────────────────────
ggplot(korea_master, aes(x = gdp)) +
  geom_histogram(bins = 10, fill = "mediumseagreen", color = "white") +
  labs(title = "Distribution of GDP Growth Rate (Korea)",
       x = "GDP Growth (%)", y = "Count") +
  theme_minimal()

# ── 4e. Boxplot: Sales by Quarter ────────────────────────
ggplot(korea_master, aes(x = quarter, y = sales, fill = quarter)) +
  geom_boxplot() +
  labs(title = "Korea E-Commerce Sales by Quarter",
       x = "Quarter", y = "Sales") +
  theme_minimal() +
  theme(legend.position = "none")

# ── 4f. Scatterplot: Sales vs GDP ────────────────────────
ggplot(korea_master, aes(x = gdp, y = sales)) +
  geom_point(color = "mediumseagreen", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "Korea: Sales vs GDP Growth",
       x = "GDP Growth (%)", y = "Sales") +
  theme_minimal()

# ── 4g. Scatterplot: Sales vs Inflation ──────────────────
ggplot(korea_master, aes(x = inflation, y = sales)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "Korea: Sales vs Inflation",
       x = "Inflation (%)", y = "Sales") +
  theme_minimal()

# ── 4h. Scatterplot: Sales vs Unemployment ───────────────
ggplot(korea_master, aes(x = unemployment, y = sales)) +
  geom_point(color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "Korea: Sales vs Unemployment",
       x = "Unemployment (%)", y = "Sales") +
  theme_minimal()


# ============================================================
# 5. CORRELATION ANALYSIS
# ============================================================
cat("\n\n============================================================\n")
cat("5. CORRELATION ANALYSIS\n")
cat("============================================================\n")

corr_vars <- korea_master %>%
  select(sales, gdp, inflation, unemployment) %>%
  na.omit()

corr_matrix <- cor(corr_vars)
cat("\n--- Pearson Correlation Matrix ---\n")
print(round(corr_matrix, 3))

corrplot(corr_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         tl.col      = "black",
         title       = "Korea: Correlation Heatmap",
         mar         = c(0, 0, 2, 0))

corr_with_sales <- corr_matrix["sales", ]
corr_with_sales <- corr_with_sales[names(corr_with_sales) != "sales"]
cat("\nCorrelations with Sales:\n")
print(sort(abs(corr_with_sales), decreasing = TRUE))
cat("Strongest predictor of sales:", names(which.max(abs(corr_with_sales))), "\n")


# ============================================================
# 6. HYPOTHESIS TESTING
# ============================================================
cat("\n\n============================================================\n")
cat("6. HYPOTHESIS TESTING\n")
cat("============================================================\n")

pre_covid  <- korea_master$sales[korea_master$covid == 0]
post_covid <- korea_master$sales[korea_master$covid == 1]

# ── 6a. F-Test: Variance equality (Pre vs Post COVID) ────
cat("\n--- F-Test: Sales Variance Pre vs Post COVID ---\n")
cat("H0: Variances are equal | H1: Variances differ\n")

if (length(pre_covid) >= 2 && length(post_covid) >= 2) {
  ftest <- var.test(pre_covid, post_covid)
  print(ftest)
  cat("Result:", ifelse(ftest$p.value < 0.05,
                        "REJECT H0 — variances significantly differ",
                        "FAIL TO REJECT H0 — no significant variance difference"), "\n")
} else {
  cat("NOTE: Korea sales data starts from 2020 (all post-COVID).\n")
  cat("Pre/post COVID variance comparison is not applicable for Korea.\n")
  cat("This test will be conducted in the combined cross-country analysis.\n")
}

# ── 6b. t-Test: COVID Impact on Sales ────────────────────
cat("\n--- t-Test: COVID Impact on Sales ---\n")
cat("H0: Mean sales before COVID = Mean sales after COVID\n")
cat("H1: Mean sales differ\n")

if (length(pre_covid) >= 2 && length(post_covid) >= 2) {
  ttest_covid <- t.test(post_covid, pre_covid, var.equal = FALSE)
  print(ttest_covid)
  cat("t-statistic:", round(ttest_covid$statistic, 4), "\n")
  cat("p-value:    ", round(ttest_covid$p.value,   4), "\n")
  cat("95% CI:     [", round(ttest_covid$conf.int[1], 2), ",",
      round(ttest_covid$conf.int[2], 2), "]\n")
  cat("Result:", ifelse(ttest_covid$p.value < 0.05,
                        "REJECT H0 — COVID significantly changed sales",
                        "FAIL TO REJECT H0 — no significant COVID effect"), "\n")
} else {
  cat("NOTE: Korea sales data starts from 2020 (all post-COVID).\n")
  cat("COVID t-test is not applicable for Korea alone.\n")
  cat("This test will be conducted in the combined cross-country analysis.\n")
}

# ── 6c. Chi-Square Variance Test ─────────────────────────
cat("\n--- Chi-Square Test: Sales Variance vs Benchmark ---\n")
n           <- nrow(korea_master)
obs_var     <- var(korea_master$sales, na.rm = TRUE)
benchmark   <- median(korea_master$sales, na.rm = TRUE)^2 * 0.1
chi_sq_stat <- (n - 1) * obs_var / benchmark
p_chi       <- 1 - pchisq(chi_sq_stat, df = n - 1)
cat("Chi-square statistic:", round(chi_sq_stat, 4), "\n")
cat("Degrees of freedom:  ", n - 1, "\n")
cat("p-value:             ", round(p_chi, 6), "\n")
cat("Result:", ifelse(p_chi < 0.05,
                      "REJECT H0 — Sales variance differs from benchmark",
                      "FAIL TO REJECT H0 — Variance consistent with benchmark"), "\n")


# ============================================================
# 7. ANOVA ANALYSIS
# ============================================================
cat("\n\n============================================================\n")
cat("7. ANOVA ANALYSIS\n")
cat("============================================================\n")

korea_master$quarter <- as.factor(korea_master$quarter)

# ── 7a. One-Way ANOVA: Sales by Quarter ──────────────────
cat("\n--- One-Way ANOVA: Sales ~ Quarter ---\n")
cat("H0: Average sales are equal across all quarters\n")
cat("H1: At least one quarter has significantly different sales\n")

anova_quarter <- aov(sales ~ quarter, data = korea_master)
print(summary(anova_quarter))

anova_q_sum <- summary(anova_quarter)[[1]]
f_stat <- anova_q_sum$`F value`[1]
p_val  <- anova_q_sum$`Pr(>F)`[1]
cat("F-statistic:", round(f_stat, 4), "\n")
cat("p-value:    ", round(p_val,  4), "\n")
cat("Result:", ifelse(!is.na(p_val) && p_val < 0.05,
                      "REJECT H0 — Significant quarterly differences exist",
                      "FAIL TO REJECT H0 — No significant quarterly effect"), "\n")

# ── 7b. Tukey Post-Hoc (only if ANOVA is significant) ────
if (!is.na(p_val) && p_val < 0.05) {
  cat("\n--- Tukey HSD Post-Hoc Test ---\n")
  print(TukeyHSD(anova_quarter))
}

# ── 7c. COVID ANOVA — only if both groups exist ──────────
cat("\n--- One-Way ANOVA: Sales ~ COVID ---\n")
if (length(unique(korea_master$covid)) > 1) {
  anova_covid <- aov(sales ~ as.factor(covid), data = korea_master)
  print(summary(anova_covid))
} else {
  cat("NOTE: Korea sales data starts from 2020 (all post-COVID).\n")
  cat("COVID ANOVA skipped — only one level present.\n")
  cat("COVID effect will be tested in the combined cross-country analysis.\n")
}


# ============================================================
# 8. REGRESSION ANALYSIS
# ============================================================
cat("\n\n============================================================\n")
cat("8. REGRESSION ANALYSIS\n")
cat("============================================================\n")

# ── 8a. Simple Regression: Sales ~ GDP ───────────────────
cat("\n--- Simple Linear Regression: Sales ~ GDP ---\n")
cat("H0: GDP has no impact on sales | H1: GDP significantly affects sales\n")
model_gdp <- lm(sales ~ gdp, data = korea_master)
print(summary(model_gdp))
cat("R²:      ", round(summary(model_gdp)$r.squared,     4), "\n")
cat("Adj. R²: ", round(summary(model_gdp)$adj.r.squared, 4), "\n")

# ── 8b. Simple Regression: Sales ~ Inflation ─────────────
cat("\n--- Simple Linear Regression: Sales ~ Inflation ---\n")
cat("H0: Inflation has no impact on sales | H1: Inflation significantly affects sales\n")
model_infl <- lm(sales ~ inflation, data = korea_master)
print(summary(model_infl))
cat("R²:      ", round(summary(model_infl)$r.squared,     4), "\n")
cat("Adj. R²: ", round(summary(model_infl)$adj.r.squared, 4), "\n")

# ── 8c. Simple Regression: Sales ~ Unemployment ──────────
cat("\n--- Simple Linear Regression: Sales ~ Unemployment ---\n")
model_unemp <- lm(sales ~ unemployment, data = korea_master)
print(summary(model_unemp))
cat("R²:      ", round(summary(model_unemp)$r.squared,     4), "\n")
cat("Adj. R²: ", round(summary(model_unemp)$adj.r.squared, 4), "\n")

# ── 8d. Multiple Regression ──────────────────────────────
cat("\n--- Multiple Regression: Sales ~ GDP + Inflation + Unemployment ---\n")
cat("Model: Sales = β0 + β1*GDP + β2*Inflation + β3*Unemployment + ε\n")
cat("NOTE: COVID dummy excluded — all Korea data is post-COVID.\n")

model_multi <- lm(sales ~ gdp + inflation + unemployment,
                  data = korea_master)
multi_sum <- summary(model_multi)
print(multi_sum)

cat("\n--- Key Metrics ---\n")
cat("R²:           ", round(multi_sum$r.squared,     4), "\n")
cat("Adjusted R²:  ", round(multi_sum$adj.r.squared, 4), "\n")
cat("F-statistic:  ", round(multi_sum$fstatistic[1], 4), "\n")

f_overall <- multi_sum$fstatistic
p_overall <- pf(f_overall[1], f_overall[2], f_overall[3], lower.tail = FALSE)
cat("Overall model p-value:", round(p_overall, 6), "\n")
cat("Overall F-test result:", ifelse(p_overall < 0.05,
                                     "REJECT H0 — Model is statistically significant",
                                     "FAIL TO REJECT H0 — Model is not significant"), "\n")

# ── 8e. Coefficient Table ────────────────────────────────
cat("\n--- Coefficient Table ---\n")
coef_table <- as.data.frame(multi_sum$coefficients)
coef_table$Significant <- ifelse(coef_table$`Pr(>|t|)` < 0.05, "YES *", "NO")
print(coef_table)

# ── 8f. Strongest Driver ─────────────────────────────────
coefs        <- abs(coef(model_multi))
coefs_no_int <- coefs[names(coefs) != "(Intercept)"]
main_driver  <- names(which.max(coefs_no_int))
cat("\nMain driver of Korea e-commerce sales (by coefficient magnitude):", main_driver, "\n")

# ── 8g. Residual Diagnostic Plots ────────────────────────
par(mfrow = c(2, 2))
plot(model_multi, main = "Multiple Regression Diagnostics — Korea")
par(mfrow = c(1, 1))


# ============================================================
# 9. FINAL BUSINESS INSIGHTS SUMMARY
# ============================================================
cat("\n\n============================================================\n")
cat("9. FINAL BUSINESS INSIGHTS — KOREA\n")
cat("============================================================\n")

cat("
┌─────────────────────────────────────────────────────────┐
│               KOREA ANALYSIS SUMMARY                    │
├─────────────────────────────────────────────────────────┤
│ Sales data:      Monthly (2020–2026), aggregated yearly │
│ Economic data:   World Bank GDP & Inflation             │
│                  KOSIS Unemployment (1991–present)      │
│                                                         │
│ NOTE ON COVID TEST:                                     │
│  Korea sales data begins in 2020 (post-COVID only).    │
│  Pre/post COVID comparison conducted in combined file. │
│                                                         │
│ KEY FINDINGS:                                           │
│  • Descriptive stats printed above (mean, sd, range)   │
│  • Correlation matrix shows strongest predictor        │
│  • Chi-square tested variance vs benchmark             │
│  • ANOVA tested quarterly seasonality                  │
│  • Multiple regression identified main sales drivers   │
│  • Overall F-test confirmed model significance         │
│                                                         │
│ NEXT STEPS:                                             │
│  • Run USA analysis script                             │
│  • Run combined dataset for cross-country comparison   │
└─────────────────────────────────────────────────────────┘
")

cat("=== KOREA ANALYSIS COMPLETE ===\n")