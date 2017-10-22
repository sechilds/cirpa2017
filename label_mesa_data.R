library(tidyverse)
library(labelled)

# work through and label the data from the mesa project

# list of useful variables for the workshop
workshop_vars = c('x_id',
                  'x_idregion_y1',
                  'proi1_y1',
                  'proi2_y1',
                  'proi3_y1',
                  'proi4_y1',
                  'proi5_y1',
                  'proi6_y1',
                  'proi7_y1',
                  'proi8_y1',
                  'inf1_y1',
                  'inf2_y1',
                  'inf3_y1',
                  'enrl1_y1',
                  'enrl_epi',
                  'enrl_epi2',
                  'ad_age',
                  'ad_gender',
                  'ad_field',
                  'ad_inst1',
                  'ad_need1',
                  'ad_need2',
                  'ad_inc1',
                  'ad_inc2',
                  'ad_award1',
                  'ad_award2',
                  'ad_unmet1',
                  'ad_unmet2',
                  'ad_inc3',
                  'ad_inc4',
                  'ad_loan1',
                  'ad_grant1',
                  'ad_asset1',
                  'ad_tuition1',
                  'ad_recip1',
                  'ad_ncb1',
                  'ad_cag1',
                  'ad_loan2',
                  'ad_bursary1',
                  'ad_loan3',
                  'ad_award3',
                  'enrl1_y2'
                  )

agree_disagree = c(`Strongly Disagree` = 1,
                   `Somewhat Disagree` = 2,
                   `Neutral` = 3,
                   `Somewhat Agree` = 4,
                   `Strongly Agree` = 5,
                   `No Opinion` = 6,
                   `Refused` = 7)

provinces = c(`Newfoundland and Labrador` = 10,
              `Prince Edward Island` = 11,
              `Nova Scotia` = 12,
              `New Brunswick` = 13,
              `Quebec` = 24,
              `Ontario` = 35,
              `Manitoba` = 46,
              `Saskatchewan` = 47,
              `Alberta` = 48,
              `British Columbia` = 59,
              `Yukon` = 60,
              `Northwest Territories` = 61,
              `Nunavut` = 62)

gender_lbl = c(`Female` = 0, `Male` = 1)

df <- df %>% set_variable_labels(x_id = "Survey ID",
                      sib2_y2 = "Have any of these brothers or sisters attended university or community college?")

df <- df %>% set_variable_labels(x_id = "Survey ID",
                      sib1_y1 = "How many brothers and sisters do you have that are older or the same age as you? Include half-, step- and adoptive brothers and sisters",
                      sib2_y2 = "Have any of these brothers or sisters attended university or community college?",
                      sib3_y1 = "How many brothers and sisters do you have that are younger than you? Include half-, step and adoptive brothers and sisters",
                      par1_y1 = "Who were the parents or guardians that you lived with MOST of the time during HIGH SCHOOL? Was it")

df <- read_csv('data/cmsf-mesa-E-2005-08_F1.csv')
df_clean <- df %>% set_variable_labels(x_id = "Survey ID",
                      cit1_y1 = "Are you a Canadian citizen?",
                      cit2_y1 = "Are you a laanded immigrant?",
                      ori1_y1 = "In what country were you born?",
                      ori2_y1 = "In what year did you come to Canada to live permanently?",
                      eth1_y1 = "People in Canada come from many different cultural or racial backgrounds. Could yo udescribe your background:",
                      lang1_y1 = "What language do you speak when you are at home with your parents?",
                      sib1_y1 = "How many brothers and sisters do you have that are older or the same age as you? Include half-, step- and adoptive brothers and sisters",
                      sib2_y1 = "Have any of these brothers or sisters attended university or community college?",
                      sib3_y1 = "How many brothers and sisters do you have that are younger than you? Include half-, step and adoptive brothers and sisters",
                      par1_y1 = "Who were the parents or guardians that you lived with MOST of the time during HIGH SCHOOL? Was it",
                      par2_y1 = "You indicated that you mostly lived with just one of your parents during high school. How frequently did you have contact with your other parent?",
                      paed1_y1 = "What was the highest level of education completed by your female guardian?",
                      paed2_y1 = "What was the highest level of education completed by your male guardian?",
                      dwel1_y1 = "Do you currently live...",
                      proi1_y1 = "Even if a person has to go deep into debt to get a PSE, it will still likely be worth it in the long run in terms of a better job and higher salary.",
                      proi2_y1 = "The time and money put into a PSE is a good investment in today's job market.",
                      proi3_y1 = "People who have a PSE get jobs that are much more satisfying.",
                      proi4_y1 = "The best way to get a prestigious job is through PSE.",
                      proi5_y1 = "I'm not sure that a PSE would pay off even in the long run, given how costly it is these days.",
                      proi6_y1 = "People would be better off putting their money into investments like real estate and the stock market than bothering with a PSE",
                      proi7_y1 = "You can learn enough about the real world without a PSE",
                      proi8_y1 = "Good jobs can be found without a PSE",
                      inf1_y1 = "Most of my friends think it's important to get PSE",
                      inf2_y1 = "My parents would be very disappointed in my if I didn't get a university or college degree",
                      inf3_y1 = "I have role models at home or at school that reprsent where I hope to go in my career based on my schooling",
                      enrl1_y1 = "Are you still enrolled in College or University?",
                      enrl_epi = "CIP Program Code",
                      enrl_epi2 = "Two Digit CIP Code",
                      ad_age = "Age",
                      ad_gender = "Gender",
                      ad_field = "Field of Study",
                      ad_inst1 = "Institution Code",
                      ad_need1 = "Value of Assessed Need",
                      ad_need2 = "Assessed Need - Total Millenium Bursary",
                      ad_inc1 = "Spousal Income",
                      ad_inc2 = "Parental Income",
                      ad_award1 = "Total Student Aid",
                      ad_award2 = "Total Aid with Bursary",
                      ad_unmet1 = "Unmet Need",
                      ad_unmet2 = "Unmet Need with Bursary",
                      ad_inc3 = "Reported Previous Year Income",
                      ad_inc4 = "Expected Work Income",
                      ad_loan1 = "Loan Amount",
                      ad_grant1 = "Grant Amount",
                      ad_asset1 = "Student Assets",
                      ad_tuition1 = "Tuition",
                      ad_recip1 = "Recipient status",
                      ad_ncb1 = "National Child Benefit",
                      ad_cag1 = "Canada Access Grant Status",
                      ad_loan2 = "Bursary loan remission",
                      ad_bursary1 = "Bursary cash award",
                      ad_loan3 = "Loan amount minus loan reduction",
                      ad_award3 = "Total award minus loan reduction",
                      enrl1_y2 = "Are you still enrolled in college or university (year 2)") %>%
  add_value_labels(proi1_y1 = agree_disagree,
                   proi2_y1 = agree_disagree,
                   proi3_y1 = agree_disagree,
                   proi4_y1 = agree_disagree,
                   proi5_y1 = agree_disagree,
                   proi6_y1 = agree_disagree,
                   proi7_y1 = agree_disagree,
                   proi8_y1 = agree_disagree,
                   inf1_y1 = agree_disagree,
                   inf2_y1 = agree_disagree,
                   inf3_y1 = agree_disagree,
                   x_idregion_y1 = provinces,
                   ad_gender = gender_lbl) %>%
  mutate(link2_y1 = ifelse(is.na(link2_y1), 1, link2_y1)) %>%
  filter(link2_y1!=2) %>%
  filter(enrl1_y1==1) %>%
  filter(!is.na(enrl1_y2)) %>%
  select(one_of(workshop_vars))

df %>% mutate(proi_scale = proi1_y1 +
                proi2_y1 +
                proi3_y1 +
                proi4_y1 +
                (6 - proi5_y1) +
                (6 - proi6_y1) +
                (6 - proi7_y1) +
                (6 - proi8_y1)) %>%
  select(proi_scale) %>%
  summary()

df_clean <- df_clean %>% mutate(proi_scale = proi1_y1 +
                proi2_y1 +
                proi3_y1 +
                proi4_y1 +
                (6 - proi5_y1) +
                (6 - proi6_y1) +
                (6 - proi7_y1) +
                (6 - proi8_y1))


vis_dat(df_clean)

df_clean %>%
  group_by(enrl1_y2) %>%
  tally()

df_clean %>%
  select(matches("inf[123]._y1"))

df_clean %>%
  mutate_at(matches("inf[123]{1}_y1"), as_factor)

df_clean %>%
  mutate_at(vars(matches("inf[123]{1}_y1")), as_factor) %>%
  model_matrix(enrl1_y1 ~ proi_scale + inf1_y1)

df_clean %>%
  mutate_at(vars(matches("inf[123]{1}_y1")), as_factor) %>%
  model_matrix(enrl1_y1 ~ proi_scale + inf1_y1)
model_matrix(df_clean, enrl1_y2 ~ proi_scale + as_factor(inf1_y1))

inf_factor <- df_clean %>%
  mutate_at(vars(matches("inf[123]{1}_y1")), as_factor)

model_matrix(df, enrl1_y2 ~ proi_scale)
model_matrix(df, ~as_factor(proi1_y1))
mod1 <- lm(enrl1_y2 ~ as_factor(proi1_y1), data=inf_factor)
summary(mod1)
broom::tidy(mod1)

#list the columns in the data frame
spec(df)

# have a look at the data for a categorical variable.
# generally these numeric ones don't have variable labels at all.
df %>%
  group_by(lang1_y1) %>%
  tally()

#So, in order to convert them to factors, you need to actually transform them.
#I will probably have to write some transformation code and set it up.

# This one is a 5 point likert scale
df %>%
  group_by(proi1_y1) %>%
  tally()

# These are the retention variables
df$enrl1_y1
df$enrl1_y2
df$enrl1_y3

# get the number of observations in my eductation proiority scale & y2 enrolment
# I filter out the NAs because those are non-respondents to Y2
df %>%
  filter(!is.na(enrl1_y2)) %>%
  group_by(proi1_y1, enrl1_y2) %>%
  tally()

# We do a similar thing just using the summarise function
df %>%
  filter(!is.na(enrl1_y2)) %>%
  filter(!is.na(proi1_y1)) %>%
  group_by(proi1_y1, enrl1_y2) %>%
  summarise(freq = n())

# then we turn that observation count into a percentage
# the trick here is that the data remain GROUPED after the group_by
# so the sum() function really looks at things within that group.
df %>%
  filter(!is.na(enrl1_y2)) %>%
  filter(!is.na(proi1_y1)) %>%
  group_by(proi1_y1, enrl1_y2) %>%
  summarise(freq = n()) %>%
  mutate(freq = freq/sum(freq) * 100)

# Here's the same code with an ungroup() inserted,
# so we can look at the percentages over the whole thing.
df %>%
  filter(!is.na(enrl1_y2)) %>%
  filter(!is.na(proi1_y1)) %>%
  group_by(proi1_y1, enrl1_y2) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  mutate(freq = freq/sum(freq) * 100)

# Or we can just look at the proportion of returners in each group.
# it does look like there is a bit of a relationship there!
df %>%
  filter(!is.na(enrl1_y2)) %>%
  filter(!is.na(proi1_y1)) %>%
  group_by(proi1_y1, enrl1_y2) %>%
  summarise(freq = n()) %>%
  mutate(freq = freq/sum(freq) * 100) %>%
  filter(enrl1_y2==1) %>%
  select(proi1_y1, freq)

# running some t-tests
# lets see if there are differences between returners are not
# so let's put together two different vectors

returners <- df %>%
  filter(enrl1_y2==1 & !is.na(proi1_y1)) %>%
  select(proi1_y1)

nonreturners <- df %>%
  filter(enrl1_y2==2 & !is.na(proi1_y1)) %>%
  select(proi1_y1)

# so, these are tibbles
# so lets pop out the individual column

returners <- returners$proi1_y1
nonreturners <- nonreturners$proi1_y1

# if you try to get the means of these, it doesn't work
# until you convert them to just vectors.
mean(returners)
mean(nonreturners)

t.test(returners, nonreturners)

# you don't need to create separate vectors
# you can get dplyr to do that for you!!
# note the use of formula syntax AND
# the rest of it!
df %>%
  filter(!is.na(enrl1_y2) & !is.na(proi1_y1)) %>%
  t.test(proi1_y1 ~ enrl1_y2, data=.)

# The nice thing about the fact that you can pipe things into t.test
# is that now you can actually do that for particular groups of
# your data
# the problem is -- in order to aggregate the results, you need data frames
df %>%
  filter(!is.na(enrl1_y2) & !is.na(proi1_y1)) %>%
  group_by(x_idregion_y1) %>%
  do(t.test(proi1_y1 ~ enrl1_y2, data=.))

# that's where the broom package comes in -- it turns your results into a data frame
df %>%
  filter(!is.na(enrl1_y2) & !is.na(proi1_y1)) %>%
  group_by(x_idregion_y1) %>%
  do(broom::tidy(t.test(proi1_y1 ~ enrl1_y2, data=.)))

df %>%
  filter(enrl1_y1==1 & !is.na(enrl1_y2)) %>%
  dim()
