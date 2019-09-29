library(tidyverse)
library(data.table)
library(viridis)
library(scales)

# Import and merge data
folder <- '/Users/billysloan/Desktop/Data_Science/Projects/bc_tax/'
lf_data <- list.files(path = folder, pattern = '*.csv')

lf_list <- lapply(lf_data, select = c('SURVYEAR', 'SURVMNTH', 'PROV', 'AGE_12', 'SEX', 
                  'MARSTAT', 'EDUC90', 'TENURE', 'HRLYEARN', 'UNION', 'FIRMSIZE'),
                  fread, sep = ',')

df <- rbindlist(lf_list)

# Rename variables
df <- rename(df, 'AGE' = 'AGE_12', 'EDUC' = 'EDUC90', 'YEAR' = 'SURVYEAR', 'MONTH' = 'SURVMNTH')

# Set wages to correct scale
df$HRLYEARN <- df$HRLYEARN / 100

# Count NAs by variable
colSums(is.na(df))

# TENURE, HRLYEARN, UNION, and FIRMSIZE are only variables with NAs
# The latter three have the same number of NAs
  
# Examine NA distribution by year and month
na_dist <- df %>%
  group_by(YEAR, MONTH) %>%
  summarize(TENURE_NAs = sum(is.na(TENURE)), HRLYEARN_NAs = sum(is.na(HRLYEARN)),
            UNION_NAs = sum(is.na(UNION)), FIRMSIZE_NAs = sum(is.na(FIRMSIZE)))


# Confirm HRLYEARN, UNION, and FIRMSIZE have same number of NAs each period
cor(na_dist$HRLYEARN_NAs, na_dist$UNION_NAs)
cor(na_dist$HRLYEARN_NAs, na_dist$FIRMSIZE_NAs)

# Both correlations = 1, so yes

# Plot NA distribution, show similar NA count for each period
na_dist %>%
  gather(Variable, Count, TENURE_NAs, HRLYEARN_NAs) %>%
  ggplot(aes(YEAR, Count, color = Variable)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE)

range(na_dist$HRLYEARN_NAs)
range(na_dist$TENURE_NAs)

# Hence, removing NAs will not heavily skew monthly observations

# Remove NAs and confirm none remain
df <- drop_na(df)
sum(is.na(df))

# Dummy variables
df <- df %>%
  mutate(BC = ifelse(PROV == 59, 1, 0),
         married = ifelse(MARSTAT == 1, 1, 0),
         post = ifelse(YEAR == 2008 & MONTH >= 7 | YEAR >= 2009, 1, 0),
         male = ifelse(SEX == 1, 1, 0),
         low_ed = ifelse(EDUC <= 1, 1, 0),
         med_ed = ifelse(between(EDUC, 2, 4), 1, 0),
         high_ed = ifelse(EDUC >= 5, 1, 0)
         )


    