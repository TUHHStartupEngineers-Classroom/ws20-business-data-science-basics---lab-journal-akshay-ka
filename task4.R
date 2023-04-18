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