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


## 2.Recent patent activity

library(vroom)
library(data.table)
library(tidyverse)
library(lubridate)
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
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_character()
)
patent_tbl <- vroom(
  file       = "02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
Join_tbl <- merge(patent_assignee_tbl,assignee_tbl, by.x = "assignee_id", by.y = "id") 
Join_1_tbl <- merge(patent_tbl,Join_tbl, by.x="id", by.y ="patent_id") 
Filter_2019_tbl <- Join_1_tbl %>%
  select("id", "date", "country","organization")%>%
  filter(between(date,as.Date("2019-01-01"), as.Date("2020-01-01")))%>%
  count(organization)%>%
  group_by(organization)    
Final_tbl <- Filter_2019_tbl %>%
  select (organization,n)%>%
  arrange(desc(n))
#list of 10 organisation 
head(Final_tbl,10)
