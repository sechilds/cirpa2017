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
