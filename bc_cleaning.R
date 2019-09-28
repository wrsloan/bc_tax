library(tidyverse)
library(data.table)

# Import and merge data
folder <- '/Users/billysloan/Desktop/Data_Science/Projects/bc_tax/lf_data/'
lf_data <- list.files(path = folder, pattern = '*.csv')

lf_list <- lapply(lf_data, select = c('SURVYEAR', 'SURVMNTH', 'PROV', 'AGE_12', 'SEX', 
                  'MARSTAT', 'EDUC90', 'TENURE', 'HRLYEARN', 'UNION', 'FIRMSIZE'),
                  fread, sep = ',')

df <- rbindlist(lf_list)

# Rename variables
df <- rename(df, 'AGE' = 'AGE_12', 'EDUC' = 'EDUC90', 'YEAR' = 'SURVYEAR', 'MONTH' = 'SURVMNTH')

# Count NAs by variable
total_na <- df %>%
  summarize(YEAR_NAs = sum(is.na(YEAR)), MONTH_NAs = sum(is.na(MONTH)), PROV_NAs = sum(is.na(PROV)), 
            AGE_NAs = sum(is.na(AGE)), SEX_NAs = sum(is.na(SEX)),
            MARSTAT_NAs = sum(is.na(MARSTAT)), EDUC_NAs = sum(is.na(EDUC)),
            TENURE_NAs = sum(is.na(TENURE)), HRLYEARN_NAs = sum(is.na(HRLYEARN)),
            UNION_NAs = sum(is.na(UNION)), FIRMSIZE_NAs = sum(is.na(FIRMSIZE)))

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
  geom_smooth()

range(na_dist$HRLYEARN_NAs)
range(na_dist$TENURE_NAs)

# Hence, removing NAs will not heavily skew monthly observations

# Remove NAs
df <- drop_na(df)

# Confirm none remain
sum(is.na(df))

    