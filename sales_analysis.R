# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

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
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
sales_by_year_state_tbl%>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
sales_by_year_state_tbl%>%
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


#Data Acquisistion

##Challenge 1 - to get data via api
library(tidyverse)
library(httr)
library(jsonlite)

url= "https://api.coinpaprika.com/v1/coins/btc-bitcoin"
resp <- GET(url)
rawToChar(resp$content)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

##Challenge 2 - competitor web scraping
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library (dplyr)
url_home = "https://www.rosebikes.de/fahrräder/mtb"
html_home         <- read_html(url_home)
type_name <- html_home %>%
  html_nodes(css = ".catalog-category-bikes__title-text")%>%
  html_text()

type_price <-  html_home %>%
  html_nodes(css = ".catalog-category-bikes__price-title")%>%
  html_text()
name_list<-c()
price_list<-c()
#type_name_list= str_split(type_name," ")
for (i in type_name){
  
  elem<-str_extract(i,".+(?=\\n)")
  name_list<-c(name_list,elem)
}
for (i in type_price){
  
  elem<-str_extract(i,"(?<= ).+(?=\\S)")
  price_list<-c(price_list,elem)
  
}
#bike_table<- enframe(name =
#df <- ldply (price_list, data.frame)
bike_price_table<-data.frame(name_list,price_list)
bike_price_table
#a<-enframe(c(name=name_list,values=price_list))


# Data Wrangling
## 1. Patent Dominance
library(vroom)
library(data.table)
library(tidyverse)
col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
Join_tbl <- merge(patent_assignee_tbl,assignee_tbl, by.x = "assignee_id", by.y = "id") 
num_tbl<- Join_tbl%>%
  select(patent_id,organization)%>% 
  count(organization)%>%
  group_by(organization) 
final_tbl <- num_tbl %>%
  select (organization,n)%>%
  arrange(desc(n))
#List of top ten companies with most assigned/granted patents.
head(final_tbl,10)


# Data Visualization

library(tidyverse)
library(vroom)
library(tictoc)
library(data.table)
library(ggplot2)
library(scales)
library(lubridate)
library(maps)

## Challenge : Cumulative Covid Plot
### Import Covid Data

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_by_month_tbl <- covid_data_tbl %>%
  select(cases_weekly, countriesAndTerritories, dateRep) %>%
  filter(countriesAndTerritories %in% c("Germany","United_Kingdom","France","Spain","United_States_of_America")) %>%
  mutate(date       = lubridate::dmy(dateRep)) %>%
  #mutate(date_floor  = floor_date(date, unit ="month")) %>%
  #mutate(month = month(date)) %>%
  group_by(countriesAndTerritories, date) %>%
  summarise(total_cases = cumsum(cases_weekly)) %>%
  ungroup() 

library(ggplot2)
library(scales)
covid_by_month_tbl%>%
  ggplot(aes(x = date, y = total_cases, color = countriesAndTerritories)) +
  geom_line(size = 0.5) +
  expand_limits(y = 0) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = " M")) +
  #scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
  #                                                  prefix = "",
  #                                               suffix = "M "))
  
  labs(title = "Covid19 confirmed cases worldwide",
       x = "Year 2020",
       y = "Cumulitive cases")


## Challenge 2 : Mortality Rate on world map
world <- map_data("world")
covid_by_mortality_tbl <- covid_data_tbl %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))%>%
  group_by(countriesAndTerritories, popData2019, deaths_weekly) %>%
  summarise(total_pop = max(popData2019))%>%
  summarise(total_death = sum(deaths_weekly))%>%
  summarise(mortality =  (total_death)/(popData2019))

class(covid_by_mortality_tbl)
setDT(covid_by_mortality_tbl)
class(covid_by_mortality_tbl)
covid_by_mortality_tbl %>% glimpse()
setDT(world)
world %>% glimpse()

tic()
covid_by_map_tbl <- merge(x = world, y = covid_by_mortality_tbl, 
                          by.x = "region", by.y = "countriesAndTerritories",
                          all.x = TRUE, 
                          all.y = FALSE)


toc()
covid_by_map_tbl%>% glimpse()

setkey(covid_by_map_tbl, "region")
key(covid_by_map_tbl)
?setorder(region, -mortality, long, lat)
setorderv(covid_by_map_tbl, c("mortality", "region", "long", "lat"), order = -1)

library(ggplot2)
library(scales)
covid_by_map_tbl%>%
  ggplot() +
  geom_map(aes(x = long, y = lat, map_id = region, fill = mortality),map = world) +
  scale_fill_continuous(labels = scales::percent)+
  labs(title = "Confirmed Covid19 deaths relative to size of the population ",
       subtitle = "More than 1.2 Million confirmed covid19 deaths worldwide") 