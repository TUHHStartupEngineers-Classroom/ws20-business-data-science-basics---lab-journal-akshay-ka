# Data Wrangling

library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)
library(vroom)

## Importing Data
assignee_col_types <- list(
  id = col_character(),
  type = col_integer(),
  name_first= col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_data/03_patent/assignee.tsv", 
  delim      = "\t", 
  col_types  = assignee_col_types,
  na         = c("", "NA", "NULL")
)

assignee_tbl
setDT(assignee_tbl)
patent_assignee_col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/03_patent/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = patent_assignee_col_types,
  na         = c("", "NA", "NULL")
)

patent_assignee_tbl
setDT(patent_assignee_tbl)
tic()
patent_assignee_merged_tbl <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                                    by.x = "assignee_id", by.y = "id",
                                    all.x = TRUE, 
                                    all.y = TRUE)
toc()
patent_assignee_merged_tbl
setkey(patent_assignee_merged_tbl, "type")
key(patent_assignee_merged_tbl)
setorderv(patent_assignee_merged_tbl, c("type", "organization"))

## Patent Domination by Companies in USA

patent_assignee_merge_tbl_usa <- patent_assignee_merged_tbl[ (type == 2)]
patent_assignee_merge_tbl_usa
tic()
most_patents_usa <- patent_assignee_merge_tbl_usa[!is.na(organization), .N, by = organization]
toc()
setkey(most_patents_usa, "organization")
key(most_patents_usa)
setorderv(most_patents_usa, c("N", "organization"), order = -1)
as_tibble(most_patents_usa, .rows = 10)

## Company with highest number of patents 

col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_integer(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  abstract = col_skip(),
  kind = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip(),
  title = col_skip()
)
patent_tbl <- vroom(
  file       = "00_data/03_patent/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)
setDT(patent_tbl)
tic()
patent_assignee_merge2_tbl <- merge(x = patent_assignee_merged_tbl, y = patent_tbl,
                                    by.x = "patent_id", by.y = "id",
                                    all.x = TRUE,
                                    all.y = TRUE)
toc()
patent_assignee_merge2_tbl 

## Company with highest patent granted in 2019

patent_assignee_merge2_tbl_usa <- patent_assignee_merge2_tbl[ (type == '2') ]
highest_patent_year <- patent_assignee_merge2_tbl_usa %>%
  select(organization, num_claims, date) %>%
  mutate(year = year(date))
highest_patent_year_2019 <- highest_patent_year[ (year == '2019') ]
setkey(highest_patent_year_2019, "organization")
key(highest_patent_year_2019)
setorderv(highest_patent_year_2019, c("num_claims", "organization"), order = -1)
task_2_ans <- highest_patent_year_2019 %>%
  select(organization, num_claims, date)
as_tibble(task_2_ans, .rows = 10)

