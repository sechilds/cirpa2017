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