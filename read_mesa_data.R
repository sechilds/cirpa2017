library(tidyverse)
library(haven)

#df <- read_sav('data/cmsf-mesa-E-2005-08_F1.sav')
df <- read_csv('data/cmsf-mesa-E-2005-08_F1.csv')

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
