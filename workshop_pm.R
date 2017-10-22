# load tidyverse, haven and labelled
library(tidyverse)
library(haven)
library(labelled)
library(readxl)
library(visdat)

# loading data --

rdm <- read_excel('data/uOttawa_RDM_Survey_Data_Anonymized.xlsx')

# simple data viz

vis_dat(rdm)

# loading data - SPSS file

nsse <- read_sav('data/NSSE17 Data (Canadian Institution).sav')

vis_dat(nsse)

# get survey metadata

questions <- nsse %>%
  var_label() %>% # labelled package
  as_tibble() %>% # from dplyr - turns it into a WIDE tibble
  gather(field_name, question_text)

write_csv(questions, 'nsse_question_list.csv')

my_function()

# questions as text

nsse %>%
  select(1:174) %>% # select only the questions
  mutate_all(to_character)

question_means <- nsse %>%
  select(1:175) %>% # include the student ID
  gather(field_name, response, -studentID) %>%
  filter(!is.na(response)) %>% # remove NA rows
  group_by(field_name) %>%
  mutate(response = as.integer(response)) %>%
  summarise(mean = mean(response))

nice_means <- inner_join(questions, question_means,
                         by='field_name')

# this didn't actually work.
nice_means <- nice_means %>%
  mutate(question_text = paste0(question_text, " (", 
                                
                                questions, ")"))



# working with MESA data!!

mesa <- read_csv('mesa_data_clean.csv')

vis_dat(mesa)
vis_dat(df_clean)

my_test <- t.test(proi_scale ~ ad_gender, data=df_clean)
broom::tidy(t.test(proi_scale ~ ad_gender, data=df_clean))

df_clean %>%
  group_by(x_idregion_y1, ad_gender)

my_tests <- df_clean %>%
  group_by(x_idregion_y1) %>%
  do(broom::tidy(t.test(proi_scale ~ ad_gender, data=.)))

# linear model

df_factor <- df_clean %>%
  mutate_if(is.labelled, as_factor) %>% 
  mutate(outcome = enrl1_y2 - 1)

mod1 <- lm(enrl1_y2 ~ x_idregion_y1, data=df_factor)

mod2 <- glm(outcome ~ x_idregion_y1, 
            data = df_factor, 
            family = binomial(link = 'logit'))
summary(mod2)

mod3 <- glm(outcome ~ x_idregion_y1 + proi_scale, 
            data = df_factor, 
            family = binomial)
summary(mod3)

df_factor %>%
  group_by(x_idregion_y1) %>%
  do(broom::tidy(glm(outcome ~proi_scale, 
                     data = ., 
                     family = binomial)))
