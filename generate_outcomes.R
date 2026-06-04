library(tidyverse)
library(corrplot)

out_dir <- "outcomes_assets"
dir.create(out_dir, showWarnings = FALSE)

clean_names <- function(df) {
  df <- df[, !is.na(names(df)) & names(df) != "", drop = FALSE]
  names(df) <- make.names(names(df), unique = TRUE)
  df
}

load_world_bank_indicator <- function(file_path, country_name, value_name) {
  read.csv(file_path, skip = 4, check.names = FALSE, stringsAsFactors = FALSE) %>%
    clean_names() %>%
    filter(Country.Name == country_name) %>%
    select(Country.Name, starts_with("X")) %>%
    pivot_longer(-Country.Name, names_to = "Year", values_to = "Value") %>%
    mutate(
      Year = as.integer(sub("^X", "", Year)),
      Value = as.numeric(Value)
    ) %>%
    filter(!is.na(Value)) %>%
    select(Year, Value) %>%
    rename(!!value_name := Value)
}

save_corrplot <- function(df, file_path, title_text) {
  png(file_path, width = 1400, height = 1200, res = 160)
  corrplot(cor(df, use = "complete.obs"), method = "color", type = "upper",
           addCoef.col = "black", tl.col = "black", mar = c(0, 0, 2, 0))
  title(title_text)
  dev.off()
}

save_plot <- function(plot_obj, file_path, width = 9, height = 6) {
  ggsave(file_path, plot = plot_obj, width = width, height = height, dpi = 160)
}

# --------------------------
# Korea
# --------------------------
korea_sales_raw <- read.csv("korea/Sales_by_retail_business_type_20260602164813.csv",
                            check.names = FALSE, stringsAsFactors = FALSE)
names(korea_sales_raw) <- gsub(" p\\)", "", names(korea_sales_raw))
names(korea_sales_raw)[1] <- "Label"

korea_monthly <- korea_sales_raw %>%
  filter(grepl("not in stores", Label, ignore.case = TRUE)) %>%
  pivot_longer(-Label, names_to = "YearMonth", values_to = "Sales") %>%
  mutate(
    Year = as.integer(sub("\\..*", "", YearMonth)),
    Month = as.integer(sub(".*\\.", "", YearMonth)),
    Quarter = factor(paste0("Q", ceiling(Month / 3)), levels = paste0("Q", 1:4)),
    Sales = as.numeric(Sales)
  ) %>%
  filter(Year >= 2020, Year <= 2024)

korea_annual <- korea_monthly %>%
  group_by(Year) %>%
  summarise(Sales = sum(Sales, na.rm = TRUE), .groups = "drop")

korea_gdp <- load_world_bank_indicator("korea/korea_gdp.csv", "Korea, Rep.", "GDP")
korea_inflation <- load_world_bank_indicator("korea/korea_inflation.csv", "Korea, Rep.", "Inflation")
korea_unemployment <- read.csv("korea/korea_unemployment.csv", header = FALSE, stringsAsFactors = FALSE) %>%
  setNames(c("Year", "Unemployment")) %>%
  mutate(
    Year = as.integer(gsub('"', "", Year)),
    Unemployment = as.numeric(Unemployment)
  )

korea_df <- korea_annual %>%
  inner_join(korea_gdp, by = "Year") %>%
  inner_join(korea_inflation, by = "Year") %>%
  inner_join(korea_unemployment, by = "Year")

save_plot(
  ggplot(korea_annual, aes(Year, Sales)) +
    geom_line(color = "#1f77b4", linewidth = 1.1) +
    geom_point(color = "#1f77b4", size = 2.5) +
    labs(title = "Korea Sales Trend", x = "Year", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "korea_trend.png")
)

save_plot(
  ggplot(korea_monthly, aes(Quarter, Sales, fill = Quarter)) +
    geom_boxplot() +
    labs(title = "Korea Sales by Quarter", x = "Quarter", y = "Sales") +
    theme_minimal() + theme(legend.position = "none"),
  file.path(out_dir, "korea_quarter_box.png")
)

save_plot(
  ggplot(korea_df, aes(GDP, Sales)) +
    geom_point(color = "#2ca02c", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(title = "Korea: Sales vs GDP", x = "GDP", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "korea_gdp_scatter.png")
)

save_corrplot(
  korea_df %>% select(Sales, GDP, Inflation, Unemployment),
  file.path(out_dir, "korea_corr.png"),
  "Korea Correlation Heatmap"
)

# --------------------------
# USA
# --------------------------
us_sales_raw <- read.csv("us_sales_forecast.csv", check.names = FALSE, stringsAsFactors = FALSE)
names(us_sales_raw)[1] <- "Label"
us_annual <- us_sales_raw %>%
  filter(grepl("Retail and food services sales, total", Label, ignore.case = TRUE)) %>%
  pivot_longer(-Label, names_to = "Year", values_to = "Sales") %>%
  mutate(
    Year = as.integer(Year),
    Sales = as.numeric(gsub(",", "", Sales))
  ) %>%
  filter(Year >= 2000, Year <= 2025)

us_gdp <- read.csv("us/us_gdp.csv", skip = 4, check.names = FALSE, stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(Country.Name == "United States") %>%
  select(Country.Name, starts_with("X")) %>%
  pivot_longer(-Country.Name, names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.integer(sub("^X", "", Year)), GDP = as.numeric(GDP)) %>%
  filter(!is.na(GDP)) %>%
  select(Year, GDP)

us_inflation <- read.csv("us/inflation rate us.csv", stringsAsFactors = FALSE) %>%
  setNames(c("Date", "Inflation")) %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Year = as.integer(format(Date, "%Y")),
    Inflation = as.numeric(Inflation)
  ) %>%
  group_by(Year) %>%
  summarise(Inflation = mean(Inflation, na.rm = TRUE), .groups = "drop")

us_unemployment <- read.csv("us/us_unemployment.csv", stringsAsFactors = FALSE) %>%
  setNames(c("Date", "Unemployment")) %>%
  mutate(
    Date = as.Date(Date),
    Year = as.integer(format(Date, "%Y")),
    Unemployment = as.numeric(Unemployment)
  ) %>%
  group_by(Year) %>%
  summarise(Unemployment = mean(Unemployment, na.rm = TRUE), .groups = "drop")

us_df <- us_annual %>%
  inner_join(us_gdp, by = "Year") %>%
  inner_join(us_inflation, by = "Year") %>%
  inner_join(us_unemployment, by = "Year")

save_plot(
  ggplot(us_annual, aes(Year, Sales)) +
    geom_line(color = "#1f77b4", linewidth = 1.1) +
    geom_point(color = "#1f77b4", size = 2.5) +
    labs(title = "USA Sales Trend", x = "Year", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "usa_trend.png")
)

save_plot(
  ggplot(us_df, aes(GDP, Sales)) +
    geom_point(color = "#2ca02c", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(title = "USA: Sales vs GDP", x = "GDP", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "usa_gdp_scatter.png")
)

save_plot(
  ggplot(us_annual %>% mutate(Period = ifelse(Year >= 2020, "Post-COVID", "Pre-COVID")),
         aes(Period, Sales, fill = Period)) +
    geom_boxplot() +
    labs(title = "USA Sales: Pre vs Post COVID", x = "", y = "Sales") +
    theme_minimal() + theme(legend.position = "none"),
  file.path(out_dir, "usa_covid_box.png")
)

save_corrplot(
  us_df %>% select(Sales, GDP, Inflation, Unemployment),
  file.path(out_dir, "usa_corr.png"),
  "USA Correlation Heatmap"
)

# --------------------------
# China
# --------------------------
china_sales_raw <- read.csv("china/sales_china.csv", check.names = FALSE, stringsAsFactors = FALSE)
names(china_sales_raw)[1] <- "Label"
china_annual <- china_sales_raw %>%
  filter(grepl("Daily Consumer Articles", Label, ignore.case = TRUE)) %>%
  pivot_longer(-Label, names_to = "Year", values_to = "Sales") %>%
  mutate(
    Year = as.integer(Year),
    Sales = as.numeric(Sales)
  ) %>%
  filter(Year >= 2009, Year <= 2025, !is.na(Sales))

china_gdp <- load_world_bank_indicator("korea/korea_gdp.csv", "China", "GDP")
china_inflation <- load_world_bank_indicator("korea/korea_inflation.csv", "China", "Inflation")
china_unemployment <- read.csv("china/china_unemployment.csv", skip = 4, check.names = FALSE, stringsAsFactors = FALSE) %>%
  clean_names() %>%
  filter(Country.Name == "China") %>%
  select(Country.Name, starts_with("X")) %>%
  pivot_longer(-Country.Name, names_to = "Year", values_to = "Unemployment") %>%
  mutate(Year = as.integer(sub("^X", "", Year)), Unemployment = as.numeric(Unemployment)) %>%
  filter(!is.na(Unemployment)) %>%
  select(Year, Unemployment)

china_df <- china_annual %>%
  inner_join(china_gdp, by = "Year") %>%
  inner_join(china_inflation, by = "Year") %>%
  inner_join(china_unemployment, by = "Year") %>%
  filter(Year >= 2020, Year <= 2024)

save_plot(
  ggplot(china_df, aes(Year, Sales)) +
    geom_line(color = "#1f77b4", linewidth = 1.1) +
    geom_point(color = "#1f77b4", size = 2.5) +
    labs(title = "China Sales Trend", x = "Year", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "china_trend.png")
)

save_plot(
  ggplot(china_df, aes(Inflation, Sales)) +
    geom_point(color = "#2ca02c", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(title = "China: Sales vs Inflation", x = "Inflation", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "china_inflation_scatter.png")
)

save_plot(
  ggplot(china_df, aes(Unemployment, Sales)) +
    geom_point(color = "#9467bd", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728") +
    labs(title = "China: Sales vs Unemployment", x = "Unemployment", y = "Sales") +
    theme_minimal(),
  file.path(out_dir, "china_unemployment_scatter.png")
)

save_corrplot(
  china_df %>% select(Sales, GDP, Inflation, Unemployment),
  file.path(out_dir, "china_corr.png"),
  "China Correlation Heatmap"
)

save_plot(
  ggplot(china_annual %>% filter(Year >= 2020, Year <= 2024) %>% mutate(Period = ifelse(Year >= 2020, "Post-COVID", "Pre-COVID")),
         aes(Period, Sales, fill = Period)) +
    geom_boxplot() +
    labs(title = "China Sales: Pre vs Post COVID", x = "", y = "Sales") +
    theme_minimal() + theme(legend.position = "none"),
  file.path(out_dir, "china_covid_box.png")
)

# --------------------------
# Comparison
# --------------------------
comparison_df <- bind_rows(
  korea_annual %>% mutate(Country = "Korea"),
  us_annual %>% filter(Year >= 2020, Year <= 2024) %>% mutate(Country = "USA"),
  china_annual %>% filter(Year >= 2020, Year <= 2024) %>% mutate(Country = "China")
)

save_plot(
  ggplot(comparison_df, aes(Country, Sales, fill = Country)) +
    geom_boxplot() +
    labs(title = "Sales Comparison Across Countries", x = "Country", y = "Sales") +
    theme_minimal() + theme(legend.position = "none"),
  file.path(out_dir, "country_boxplot.png")
)
