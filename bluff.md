## Revised Methodology and Hypotheses (Course-Aligned)

### 1) Final Research Scope
To keep the project feasible and statistically rigorous for IBT4109, we focus on three core questions using monthly Korean retail/e-commerce sales data:

1. Do sales patterns differ significantly across seasons/quarters?
2. Do product groups and retail business types show different trend and volatility patterns?
3. Are macroeconomic indicators (e.g., CCI, unemployment, CPI) associated with monthly sales variation?

This scope supports both exploratory and inferential analysis while staying manageable for a 3-member team.

### 2) Data and Unit of Analysis
Primary datasets (KOSIS):
- Sales by city/province (monthly)
- Sales by product group (monthly)
- Sales by retail business type (monthly)

Additional macro datasets (optional but recommended):
- Consumer Confidence Index (CCI)
- Unemployment rate
- CPI (for real-term adjustment)

Unit of analysis:
- Monthly observations by group (region/product/business type), reshaped to long format.

### 3) Variables
Dependent variable (Y):
- Monthly sales value (KRW), and optionally log(sales) for variance stabilization.

Independent / grouping variables:
- Quarter (Q1–Q4), Month, Year
- Product group
- Retail business type
- Region (city/province)
- Macroeconomic indicators: CCI, unemployment, CPI (if merged)

Control variables:
- Time trend (t)
- Seasonal dummies (month or quarter)

### 4) Analytical Plan (R)
Stage A: Data preprocessing
- Wide-to-long transformation (`pivot_longer`)
- Date parsing (`lubridate`)
- Category cleaning and translation
- Optional CPI deflation to real sales
- Missing-value check and transparent handling

Stage B: Descriptive EDA
- Time-series plots by group
- Seasonal boxplots by quarter/month
- Heatmaps (month × category/region)
- Summary statistics and volatility comparison (SD/CV)

Stage C: Inferential statistics
- One-way ANOVA: sales differences across quarters
- Post-hoc Tukey HSD: identify which quarters differ
- Group comparison tests (product/business type)
- Correlation analysis with macro indicators
- Multiple regression with seasonal/time controls:
  - Example: `log(Sales) ~ CCI + Unemployment + CPI + Quarter + Trend + Group FE`

Stage D: Diagnostics and robustness
- Residual normality and heteroskedasticity checks
- Multicollinearity check (VIF)
- Sensitivity check with/without COVID-period indicator (2020 shock)

### 5) Hypotheses (Testable and Dataset-Matched)
H1 (Seasonality):
- H0: Mean monthly sales are equal across quarters.
- H1: At least one quarter has a different mean monthly sales level.

H2 (Category/Type differences):
- H0: Mean sales do not differ across product groups / retail business types.
- H1: Mean sales differ significantly across at least some groups.

H3 (Macro association):
- H0: CCI, unemployment, and CPI are not associated with monthly sales after seasonal controls.
- H1: At least one macro variable is significantly associated with monthly sales after controls.

H4 (Regional heterogeneity):
- H0: Regional sales series follow the same average pattern over time.
- H1: Regional sales patterns differ significantly (level and/or trend).

### 6) Team Execution Plan (3 Members)
- Member 1 (Data Engineering Lead): data acquisition, cleaning, variable harmonization, merge pipeline.
- Member 2 (EDA & Visualization Lead): exploratory analysis, figure design, initial interpretation.
- Member 3 (Statistical Modeling Lead): ANOVA/regression/hypothesis testing, diagnostics, robustness checks.
- Joint: final report writing, managerial implications, and presentation.

### 7) Expected Contribution
This project provides a reproducible R workflow for Korean e-commerce sales analytics using public data and demonstrates how EDA plus inferential statistics can produce practical, evidence-based insights for retail planning, seasonality strategy, and market monitoring.