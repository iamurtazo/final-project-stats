# ============================================================
# USA E-COMMERCE SALES DRIVERS ANALYSIS
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
library(readxl)


# ============================================================
# 1. LOAD & RESHAPE DATA
# ============================================================

# ── 1a. Sales (Excel file) ─────────────────────────────────
# Print ALL rows to find structure
sales_raw <- read_excel("us/us_sales.xls", sheet = 1, col_names = FALSE)

cat("=== FULL SALES RAW PREVIEW ===\n")
print(sales_raw[1:10, 1:10])

# Find which row contains the 4-digit year headers
year_row_idx <- NA
for (i in 1:min(10, nrow(sales_raw))) {
  row_vals <- as.character(unlist(sales_raw[i, ]))
  if (sum(grepl("^[0-9]{4}r?$", row_vals), na.rm = TRUE) >= 3) {
    year_row_idx <- i
    break
  }
}

if (is.na(year_row_idx)) {
  stop("Could not find year header row in sales file. Check the file manually.")
}

cat("\nYear header found at row:", year_row_idx, "\n")

header_row  <- as.character(unlist(sales_raw[year_row_idx, ]))
year_cols   <- grep("^[0-9]{4}r?$", header_row)
year_labels <- header_row[year_cols]

cat("Year columns:", year_labels, "\n")

# Find the "Retail" total row (search rows after header)
retail_row_idx <- NA
for (i in (year_row_idx + 1):nrow(sales_raw)) {
  cell <- as.character(sales_raw[i, 2])
  if (!is.na(cell) && grepl("^Retail", cell, ignore.case = TRUE)) {
    retail_row_idx <- i
    break
  }
}

if (is.na(retail_row_idx)) {
  # Fallback: use the row right after the header
  retail_row_idx <- year_row_idx + 1
  cat("Warning: 'Retail' row not found, using row after header:", retail_row_idx, "\n")
} else {
  cat("Retail row found at row:", retail_row_idx, "\n")
}

retail_row <- sales_raw[retail_row_idx, ]
cat("Retail row preview:\n")
print(retail_row[1, 1:min(10, ncol(retail_row))])

us_sales <- data.frame(
  year  = as.integer(gsub("r$", "", year_labels)),
  sales = as.numeric(gsub(",", "", as.character(unlist(retail_row[1, year_cols]))))
) %>%
  filter(!is.na(sales), !is.na(year)) %>%
  arrange(year)

cat("\n=== SALES ANNUAL ===\n")
print(us_sales)

if (nrow(us_sales) == 0) {
  stop("Sales data is still empty after fix. Run: print(sales_raw[1:15, 1:10]) and check structure.")
}


# ── 1b. GDP (World Bank) ───────────────────────────────────
gdp_raw <- read.csv("us/us_gdp.csv", skip = 4)

us_gdp <- gdp_raw %>%
  filter(trimws(Country.Name) == "United States") %>%
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
print(head(us_gdp))


# ── 1c. Inflation ──────────────────────────────────────────
inflation_raw <- read.csv("us/inflation rate us.csv", stringsAsFactors = FALSE)
colnames(inflation_raw) <- c("date", "inflation")

us_inflation <- inflation_raw %>%
  mutate(
    date      = as.Date(date, format = "%m/%d/%Y"),
    year      = as.integer(format(date, "%Y")),
    inflation = as.numeric(inflation)
  ) %>%
  filter(!is.na(year), !is.na(inflation)) %>%
  group_by(year) %>%
  summarise(inflation = mean(inflation, na.rm = TRUE), .groups = "drop")

cat("\n=== INFLATION LONG ===\n")
print(head(us_inflation))


# ── 1d. Unemployment ───────────────────────────────────────
unemp_raw <- read.csv("us/us_unemployment.csv", stringsAsFactors = FALSE)
colnames(unemp_raw) <- c("date", "unemployment")

us_unemployment <- unemp_raw %>%
  mutate(
    date         = as.Date(date),
    year         = as.integer(format(date, "%Y")),
    unemployment = as.numeric(unemployment)
  ) %>%
  filter(!is.na(year), !is.na(unemployment)) %>%
  group_by(year) %>%
  summarise(unemployment = mean(unemployment, na.rm = TRUE), .groups = "drop")

cat("\n=== UNEMPLOYMENT LONG ===\n")
print(head(us_unemployment))


# ============================================================
# 2. MERGE & CREATE MASTER DATASET
# ============================================================
us_master <- us_sales %>%
  left_join(us_gdp,          by = "year") %>%
  left_join(us_inflation,    by = "year") %>%
  left_join(us_unemployment, by = "year") %>%
  mutate(
    country     = "USA",
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
cat("Sales years:       ", paste(sort(unique(us_sales$year)),        collapse = ", "), "\n")
cat("GDP years (range): ", min(us_gdp$year), "-", max(us_gdp$year), "\n")
cat("Inflation years:   ", min(us_inflation$year), "-", max(us_inflation$year), "\n")
cat("Unemployment years:", min(us_unemployment$year), "-", max(us_unemployment$year), "\n")
cat("Master rows:       ", nrow(us_master), "\n")
cat("Master years:      ", paste(sort(unique(us_master$year)), collapse = ", "), "\n")

if (nrow(us_master) == 0) {
  stop("Master dataset is empty — year ranges do not overlap. Check sales years vs other datasets.")
}

cat("\n=== MASTER DATASET ===\n")
print(str(us_master))
print(head(us_master))


# ============================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================
cat("\n\n============================================================\n")
cat("3. DESCRIPTIVE STATISTICS\n")
cat("============================================================\n")

print(summary(us_master))

desc_stats <- us_master %>%
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

ggplot(us_master, aes(x = year, y = sales)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "USA E-Commerce Sales Over Time", x = "Year", y = "Total Retail Sales (millions)") +
  theme_minimal()

ggplot(us_master, aes(x = sales)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(title = "Distribution of USA E-Commerce Sales", x = "Sales", y = "Count") +
  theme_minimal()

ggplot(us_master, aes(x = inflation)) +
  geom_histogram(bins = 10, fill = "coral", color = "white") +
  labs(title = "Distribution of Inflation Rate (USA)", x = "Inflation (%)", y = "Count") +
  theme_minimal()

ggplot(us_master, aes(x = gdp)) +
  geom_histogram(bins = 10, fill = "mediumseagreen", color = "white") +
  labs(title = "Distribution of GDP (USA)", x = "GDP", y = "Count") +
  theme_minimal()

ggplot(us_master, aes(x = covid_label, y = sales, fill = covid_label)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Pre-COVID" = "steelblue", "Post-COVID" = "tomato")) +
  labs(title = "USA E-Commerce Sales: Pre vs Post COVID", x = "", y = "Sales") +
  theme_minimal() + theme(legend.position = "none")

ggplot(us_master, aes(x = quarter, y = sales, fill = quarter)) +
  geom_boxplot() +
  labs(title = "USA E-Commerce Sales by Quarter", x = "Quarter", y = "Sales") +
  theme_minimal() + theme(legend.position = "none")

ggplot(us_master, aes(x = gdp, y = sales)) +
  geom_point(color = "mediumseagreen", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "USA: Sales vs GDP", x = "GDP", y = "Sales") +
  theme_minimal()

ggplot(us_master, aes(x = inflation, y = sales)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "USA: Sales vs Inflation", x = "Inflation (%)", y = "Sales") +
  theme_minimal()

ggplot(us_master, aes(x = unemployment, y = sales)) +
  geom_point(color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  labs(title = "USA: Sales vs Unemployment", x = "Unemployment (%)", y = "Sales") +
  theme_minimal()


# ============================================================
# 5. CORRELATION ANALYSIS
# ============================================================
cat("\n\n============================================================\n")
cat("5. CORRELATION ANALYSIS\n")
cat("============================================================\n")

corr_vars <- us_master %>%
  select(sales, gdp, inflation, unemployment) %>%
  na.omit()

cat("Rows available for correlation:", nrow(corr_vars), "\n")

if (nrow(corr_vars) < 3) {
  cat("Not enough data for correlation analysis.\n")
} else {
  corr_matrix <- cor(corr_vars)
  cat("\n--- Pearson Correlation Matrix ---\n")
  print(round(corr_matrix, 3))
  
  corrplot(corr_matrix,
           method      = "color",
           type        = "upper",
           addCoef.col = "black",
           tl.col      = "black",
           title       = "USA: Correlation Heatmap",
           mar         = c(0, 0, 2, 0))
  
  corr_with_sales <- corr_matrix["sales", ]
  corr_with_sales <- corr_with_sales[names(corr_with_sales) != "sales"]
  cat("\nCorrelations with Sales:\n")
  print(sort(abs(corr_with_sales), decreasing = TRUE))
  cat("Strongest predictor of sales:", names(which.max(abs(corr_with_sales))), "\n")
}


# ============================================================
# 6. HYPOTHESIS TESTING
# ============================================================
cat("\n\n============================================================\n")
cat("6. HYPOTHESIS TESTING\n")
cat("============================================================\n")

pre_covid  <- us_master$sales[us_master$covid == 0]
post_covid <- us_master$sales[us_master$covid == 1]

cat("\n--- F-Test: Sales Variance Pre vs Post COVID ---\n")
if (length(pre_covid) >= 2 && length(post_covid) >= 2) {
  ftest <- var.test(pre_covid, post_covid)
  print(ftest)
  cat("Result:", ifelse(ftest$p.value < 0.05,
                        "REJECT H0 — variances significantly differ",
                        "FAIL TO REJECT H0 — no significant variance difference"), "\n")
} else {
  cat("Not enough data in one group for F-test.\n")
}

cat("\n--- t-Test: COVID Impact on Sales ---\n")
if (length(pre_covid) >= 2 && length(post_covid) >= 2) {
  ttest_covid <- t.test(post_covid, pre_covid, var.equal = FALSE)
  print(ttest_covid)
  cat("Result:", ifelse(ttest_covid$p.value < 0.05,
                        "REJECT H0 — COVID significantly changed sales",
                        "FAIL TO REJECT H0 — no significant COVID effect"), "\n")
} else {
  cat("Not enough data in one group for t-test.\n")
}

cat("\n--- Chi-Square Test: Sales Variance vs Benchmark ---\n")
n           <- nrow(us_master)
obs_var     <- var(us_master$sales, na.rm = TRUE)
benchmark   <- median(us_master$sales, na.rm = TRUE)^2 * 0.1
chi_sq_stat <- (n - 1) * obs_var / benchmark
p_chi       <- 1 - pchisq(chi_sq_stat, df = n - 1)
cat("Chi-square statistic:", round(chi_sq_stat, 4), "\n")
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

us_master$quarter <- as.factor(us_master$quarter)

cat("\n--- One-Way ANOVA: Sales ~ Quarter ---\n")
anova_quarter <- aov(sales ~ quarter, data = us_master)
print(summary(anova_quarter))

anova_q_sum <- summary(anova_quarter)[[1]]
f_stat <- anova_q_sum$`F value`[1]
p_val  <- anova_q_sum$`Pr(>F)`[1]
cat("Result:", ifelse(!is.na(p_val) && p_val < 0.05,
                      "REJECT H0 — Significant quarterly differences exist",
                      "FAIL TO REJECT H0 — No significant quarterly effect"), "\n")

if (!is.na(p_val) && p_val < 0.05) {
  cat("\n--- Tukey HSD Post-Hoc Test ---\n")
  print(TukeyHSD(anova_quarter))
}

cat("\n--- One-Way ANOVA: Sales ~ COVID ---\n")
if (length(unique(us_master$covid)) > 1) {
  anova_covid <- aov(sales ~ as.factor(covid), data = us_master)
  print(summary(anova_covid))
} else {
  cat("Only one COVID level present — skipping.\n")
}


# ============================================================
# 8. REGRESSION ANALYSIS
# ============================================================
cat("\n\n============================================================\n")
cat("8. REGRESSION ANALYSIS\n")
cat("============================================================\n")

cat("\n--- Simple Linear Regression: Sales ~ GDP ---\n")
model_gdp <- lm(sales ~ gdp, data = us_master)
print(summary(model_gdp))

cat("\n--- Simple Linear Regression: Sales ~ Inflation ---\n")
model_infl <- lm(sales ~ inflation, data = us_master)
print(summary(model_infl))

cat("\n--- Simple Linear Regression: Sales ~ Unemployment ---\n")
model_unemp <- lm(sales ~ unemployment, data = us_master)
print(summary(model_unemp))

cat("\n--- Multiple Regression: Sales ~ GDP + Inflation + Unemployment + COVID ---\n")
model_multi <- lm(sales ~ gdp + inflation + unemployment + covid, data = us_master)
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

coef_table <- as.data.frame(multi_sum$coefficients)
coef_table$Significant <- ifelse(coef_table$`Pr(>|t|)` < 0.05, "YES *", "NO")
cat("\n--- Coefficient Table ---\n")
print(coef_table)

coefs        <- abs(coef(model_multi))
coefs_no_int <- coefs[names(coefs) != "(Intercept)"]
main_driver  <- names(which.max(coefs_no_int))
cat("\nMain driver of USA e-commerce sales:", main_driver, "\n")

par(mfrow = c(2, 2))
plot(model_multi, main = "Multiple Regression Diagnostics — USA")
par(mfrow = c(1, 1))


# ============================================================
# 9. FINAL SUMMARY
# ============================================================
cat("\n=== USA ANALYSIS COMPLETE ===\n")