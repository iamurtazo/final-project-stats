# ============================================================
# LOAD LIBRARIES
# ============================================================
library(tidyr)
library(dplyr)

# ============================================================
# 1. SALES DATA
# ============================================================
china_sales <- read.csv("china/sales_china.csv")

# Inspect the data
names(china_sales)
head(china_sales)

# Years are stored as columns (X2009, X2010...) — reshape to long format
china_sales <- china_sales %>%
  pivot_longer(
    cols      = starts_with("X"),  # select all year columns
    names_to  = "year",
    values_to = "sales"
  ) %>%
  mutate(year = as.numeric(sub("X", "", year)))  # X2011 → 2011

head(china_sales)



# ============================================================
# 2. GDP / INFLATION DATA
# ============================================================
china_inflation <- read.csv("china/china_inflation.csv", sep = "\t")

# Inspect the data
names(china_inflation)
head(china_inflation)
# Reshape from wide to long format
china_inflation <- china_inflation %>%
  pivot_longer(
    cols      = starts_with("Data.Source.World.Development.Indicators"),
    names_to  = "year",
    values_to = "inflation"
  ) %>%
  mutate(year = as.numeric(sub("Data.Source.World.Development.Indicators", "", year)))

head(china_inflation)





# ============================================================
# 3. UNEMPLOYMENT DATA
# ============================================================
china_unemployment <- read.csv("china/china_unemployment.csv", sep = "\t")

# Inspect — check actual column names in case they differ from "X20XX"
names(china_unemployment)
head(china_unemployment)

# Reshape from wide to long format
# Uses gsub to strip ANY non-numeric prefix (X, Y, yr, etc.) — more robust
china_unemployment <- china_unemployment %>%
  pivot_longer(
    cols      = starts_with("Data.Source.World.Development.Indicators"),   
    names_to  = "year",
    values_to = "unemployment"
  ) %>%
  mutate(year = as.numeric(sub("Data.Source.World.Development.Indicators", "", year)))

head(china_unemployment)





china_master <- china_sales %>%
  left_join(china_inflation, by = "year") %>%
  left_join(china_unemployment, by = "year")


china_master <- china_master %>%
  na.omit()
summary(china_master)
str(china_master)
china_master$country <- "China"
china_master$covid <- ifelse(china_master$year >= 2020, 1, 0)

unique(china_sales$year)
unique(china_inflation$year)
unique(china_unemployment$year)
#### DESCRIPTIVE STATISTICS

summary(china_master)
sd(china_master$sales)
mean(china_master$sales)


#####
library(ggplot2)

ggplot(china_master, aes(x=year, y=sales)) +
  geom_line()
#####
Inflation vs Sales
ggplot(china_master, aes(x=inflation, y=sales)) +
  geom_point() +
  geom_smooth(method="lm")
####
cor(china_master[, c("sales", "inflation", "unemployment")], use="complete.obs")
#####
china_master$covid <- ifelse(china_master$year >= 2020, 1, 0)

t.test(sales ~ covid, data = china_master)
#####
model_china <- lm(
  sales ~ inflation + unemployment,
  data = china_master
)

summary(model_china)
