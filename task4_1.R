
# Tidyverse
library(tidyverse)
library(vroom)
# Data Table
library(data.table)
# Counter
library(tictoc)

# 2.0 DATA IMPORT ----
library(vroom)
col_types_1 <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)
patent_tbl <- vroom(
  file       = "02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_1,
  na         = c("", "NA", "NULL")
)

# 2.0 DATA IMPORT ----
library(vroom)
col_types_2 <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_2,
  na         = c("", "NA", "NULL")
)

# 2.0 DATA IMPORT ----
library(vroom)
col_types_3 <- list(
  id = col_character(),
  type = col_double(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_3,
  na         = c("", "NA", "NULL")
)

# 2.0 DATA IMPORT ----
library(vroom)
col_types_4 <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_double()
)
uspc_tbl <- vroom(
  file       = "02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_4,
  na         = c("", "NA", "NULL")
)

# 3.1 Patent Data ----
class(patent_tbl)
setDT(patent_tbl)
class(patent_tbl)
patent_tbl %>% glimpse()
setDT(patent_assignee_tbl)
patent_assignee_tbl %>% glimpse()
setDT(assignee_tbl)
assignee_tbl %>% glimpse()
setDT(uspc_tbl)
uspc_tbl %>% glimpse()

# 4.0 DATA WRANGLING ----
# 4.1 Joining / Merging Data ----
tic()
patent_tbl_1 <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                      by.x = "assignee_id", by.y = "id",
                      all.x = TRUE, 
                      all.y = TRUE)
toc()
patent_tbl_1 %>% glimpse()
tic()
patent_tbl_2 <- merge(x = patent_tbl_1, y = patent_tbl,
                      by.x = "patent_id", by.y = "id",
                      all.x = TRUE,
                      all.y = TRUE)
toc()
patent_tbl_2 %>% glimpse()
tic()
patent_tbl_3 <- merge(x = patent_tbl_2, y = uspc_tbl,
                      by = "patent_id",
                      all.x = TRUE,  
                      all.y = TRUE)
toc()
patent_tbl_3 %>% glimpse()

# Preparing the Data Table
setkey(patent_tbl_1, "type")
key(patent_tbl_1)
?setorder()
setorderv(patent_tbl_1, c("type", "organization"))

# Preparing the Data Table
setkey(patent_tbl_2, "type")
key(patent_tbl_2)
?setorder()
setorderv(patent_tbl_2, c("type", "organization"))

# Preparing the Data Table
setkey(patent_tbl_3, "type")
key(patent_tbl_3)
?setorder()
setorderv(patent_tbl_3, c("type", "organization"))

# 5.1 Highest patents in US
patent_tbl_1_typ <- patent_tbl_1[ (type == '2'),] 
tic()
patent_US_Highest <- patent_tbl_1_typ[!is.na(organization), .N, by = organization]
toc()
setkey(patent_US_Highest, "organization")
key(patent_US_Highest)
?setorder(-N, organization)
setorderv(patent_US_Highest, c("N", "organization"), order = -1)

as_tibble(patent_US_Highest, .rows = 10)

patent_tbl_2_typ <- patent_tbl_2[ !(type == 'na') & (type == '2') ]
patent_tbl_2_typ_month <- patent_tbl_2_typ %>%
  select(organization, num_claims, date) %>%
  mutate(month = month(date))
patent_tbl_2_typ_January <- patent_tbl_2_typ_month[ (month == '1') ]
setkey(patent_tbl_2_typ_January, "organization")
key(patent_tbl_2_typ_January)
?setorder(-num_claims, organization)
setorderv(patent_tbl_2_typ_January, c("num_claims", "organization"), order = -1)

as_tibble(patent_tbl_2_typ_January, .rows = 10)

patent_tbl_3_typ <- patent_tbl_3[!(type == 'na')]
patent_tbl_3_typ <- patent_tbl_3_typ[!(mainclass_id == 'na')]
setkey(patent_tbl_3_typ, "organization")
key(patent_tbl_3_typ)
?setorder(-num_claims, organization, -mainclass_id)
setorderv(patent_tbl_3_typ, c("num_claims", "organization", "mainclass_id"), order = -1)

as_tibble(patent_tbl_3_typ, .rows = 10)
