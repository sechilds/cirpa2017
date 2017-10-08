library(tidyverse)
library(readxl)

df <- readxl::read_xlsx('data/uOttawa_RDM_Survey_Data_Anonymized.xlsx', sheet = 'Data')

df %>%
  select(NUMBER_PROJECTS) %>%
  group_by(NUMBER_PROJECTS) %>%
  tally()
