# df1 <- read.csv("Large_retail_nonspecialized_stores_sales_by_city_and_province_20260602164857.csv")

# df1

# install.packages("tidyverse")
# library(tidyverse)
# df2 <- read.csv("Large_retail_nonspecialized_stores_sales_by_city_and_province_20260602164857.csv")
# df2

# extract GDP from gdp data
gdp <- read.csv("korea/korea_gdp.csv", skip = 4, check.names = FALSE)
korea_gdp <- gdp[gdp[["Country Name"]] == "Korea, Rep.", ]
year_cols <- as.character(2000:2024)

korea_gdp <- korea_gdp[, c("Country Name", "Country Code", year_cols)]

# extracting china's gdp from gdp data
china_gdp <- gdp[gdp[["Country Name"]] == "China", ]
china_gdp <- china_gdp[, c("Country Name", "Country Code", year_cols)]

# extracting us's gdp from gdp data
us_gdp <- gdp[gdp[["Country Name"]] == "United States", ]
us_gdp <- us_gdp[, c("Country Name", "Country Code", year_cols)]

#======INFLATION=======================

# extract inflation from inflation data
inflation <- read.csv("korea/korea_inflation.csv", skip = 4, check.names = FALSE)
korea_inflation <- inflation[inflation[["Country Name"]] == "Korea, Rep.", ]
year_cols <- as.character(2000:2024)

korea_inflation <- korea_inflation[, c("Country Name", "Country Code", year_cols)]
korea_inflation

# extracting china's inflation from inflation data
china_inflation <- inflation[inflation[["Country Name"]] == "China", ]
china_inflation <- china_inflation[, c("Country Name", "Country Code", year_cols)]

# extracting us's inflation from inflation data
us_inflation <- inflation[inflation[["Country Name"]] == "United States", ]
us_inflation <- us_inflation[, c("Country Name", "Country Code", year_cols)]

# =================================
# extract UNEMPLOYMENT from unemployment data
# korea_unemployment <- read.csv("korea/korea_unemployment.csv")
# us_unemployment <- read.csv("us/us_unemployment.csv")
# china_unemployment <- read.csv("china/china_unemployment.csv")



# extract unemployment from gdp data
unemployment <- read.csv("china/china_unemployment.csv", skip = 4, check.names = FALSE)
korea_unemployment <- unemployment[unemployment[["Country Name"]] == "Korea, Rep.", ]
year_cols <- as.character(2000:2024)

korea_unemployment <- korea_unemployment[, c("Country Name", "Country Code", year_cols)]

# extracting china's unemployment 
china_unemployment <- unemployment[unemployment[["Country Name"]] == "China", ]
china_unemployment <- china_unemployment[, c("Country Name", "Country Code", year_cols)]

# extracting us's unemployment from gdp data
us_unemployment <- unemployment[unemployment[["Country Name"]] == "United States", ]
us_unemployment <- us_unemployment[, c("Country Name", "Country Code", year_cols)]

# ================E-COMMERCE SALES=======================
korea_sales <- read.csv("korea/Sales_by_product_group_20260602164655.csv")
us_sales <- read.csv("us/us_sales.csv")
china_sales <- read.csv("china/sales_china.csv")