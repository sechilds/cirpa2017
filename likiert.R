library(likert)
library(tidyverse)

mylevels = c('Strongly Disagree', 'Somewhat Disagree', 'Neutral', 'Somewhat Agree', 'Strongly Agree')

mesa %>% 
  select(starts_with("proi")) %>%
  filter_all(any_vars(is.na(.))) %>%
  mutate_all(factor) %>%
  mutate_all(fct_recode, 'Strongly Disagree' = '1',
             'Somehat Disagree' = '2',
             'Neutral' = '3',
             'Somewhat Agree' = '4',
             'Strongly Agree' = '5') %>%
  likert::likert()
  
  mutate(proi1_y1 = factor(proi1_y1, levels = mylevels))
  m.utate_all(factor, levels=levels)
  
mesa %>% 
  select(proi1_y1, proi2_y1) %>%
  mutate_all(factor)
  
t1 <-mesa %>% 
  select(proi1_y1, proi2_y1) %>%
  mutate_all(factor) %>%
  mutate_all(fct_recode, 'Strongly Disagree' = '1',
             'Somewhat Disagree' = '2',
             'Neutral' = '3',
             'Somewhat Agree' = '4',
             'Strongly Agree' = '5')

