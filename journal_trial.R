# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "Data_Science_Basics/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("Data_Science_Basics/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("Data_Science_Basics/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
challenge_bike_orderlines_wrangled_tbl  <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  mutate(total.price = price * quantity) %>%
  
  select(-...1, -gender) %>%
  
  select(-ends_with(".id")) %>%
  
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,state,city,
         everything()) %>%
  
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----


# 6.1 Sales by state ----

# Step 1 - Manipulate
library(lubridate)
sales_by_state_tbl <- challenge_bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  
  group_by(state) %>% 
  summarize(sales = sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_state_tbl %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "comparison",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and state ----

# Step 1 - Manipulate
sales_by_year_state_tbl <- challenge_bike_orderlines_wrangled_tbl %>%
  
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))


# Step 2 - Visualize
sales_by_year_state_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "A comparison",
    fill = "Main category" # Changes the legend name
  )  


# 7.0 Writing Files ----
library("writexl")

# 7.1 Excel ----
sales_by_year_state_tbl %>%
  write_xlsx("Data_Science_Basics/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
sales_by_year_state_tbl%>% 
  write_csv("DS-101/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
sales_by_year_state_tbl%>%
  write_rds("DS-101/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


#Data Acquisistion

##Challenge 1 - to get data via api
library(tidyverse)
library(httr)
library(jsonlite)

url= "https://www.startupengineer.io/_repos/dat_sci_1/03_dat_acq"
resp <- GET(url)
rawToChar(resp$content)

resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

##Challenge 2 - competitor web scraping
library(tidyverse)
library(dplyr)
library(glue)
library(jsonlite)
library(rvest)
library(stringi)
library(xopen)

url  <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"
rose_html <- url %>% 
  read_html()

names <-  rose_html %>% 
  html_nodes(css = ".catalog-category-bikes__title-text") %>% 
  html_text() %>% 
  str_extract("(?<= ).*(?=)") %>%
  as_tibble()
names

price <-  rose_html %>% 
  html_nodes(css = ".catalog-category-bikes__price-title") %>% 
  html_text() %>% 
  str_extract("(?<= ).*(?=)") %>%
  as_tibble()
price
