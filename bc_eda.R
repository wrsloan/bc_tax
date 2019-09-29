# Average HRLYEARN by education level over time
df %>%
  group_by(YEAR, MONTH) %>%
  summarize(Low_Ed = mean(HRLYEARN[low_ed == 1]),
            Med_Ed = mean(HRLYEARN[med_ed == 1]),
            High_Ed = mean(HRLYEARN[high_ed == 1])) %>%
  gather(Educ_Level, Hourly_Earnings, Low_Ed, Med_Ed, High_Ed) %>%
  ggplot(aes(YEAR, Hourly_Earnings, color = Educ_Level)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(labels = dollar) +
  xlab(NULL)

# Separated by British Columbia vs Rest of Canada
df %>%
  select(YEAR, MONTH, HRLYEARN, low_ed, med_ed, high_ed, BC) %>%
  group_by(YEAR, MONTH, BC) %>%
  summarize(Low_Ed = mean(HRLYEARN[low_ed == 1]),
            Med_Ed = mean(HRLYEARN[med_ed == 1]),
            High_Ed = mean(HRLYEARN[high_ed == 1]),
  ) %>%
  gather(Educ_Level, Hourly_Earnings, Low_Ed, Med_Ed, High_Ed) %>%
  ggplot(aes(YEAR, Hourly_Earnings, color = Educ_Level)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(name = 'Hourly Earnings', labels = dollar) +
  xlab(NULL) +
  facet_wrap(~BC, labeller = labeller(BC = c('1' = 'British Columbia',
                                             '0' = 'Rest of Canada'))) +
  theme(legend.title = element_blank())

# BC wages grow much slower than ROC, notably from 2008 - 2016

# Plot difference in average hourly earnings from BC perspective
df %>%
  select(YEAR, MONTH, HRLYEARN, low_ed, med_ed, high_ed, BC) %>%
  group_by(YEAR, MONTH) %>%
  summarize(Low_Ed = mean(HRLYEARN[low_ed == 1 & BC == 1]) -
              mean(HRLYEARN[low_ed == 1 & BC == 0]),
            Med_Ed = mean(HRLYEARN[med_ed == 1 & BC == 1]) -
              mean(HRLYEARN[med_ed == 1 & BC == 0]),
            High_Ed = mean(HRLYEARN[high_ed == 1 & BC == 1]) -
              mean(HRLYEARN[high_ed == 1 & BC == 0])) %>%
  gather(Educ_Level, Hourly_Earnings, Low_Ed, Med_Ed, High_Ed) %>%
  ggplot(aes(YEAR, Hourly_Earnings, color = Educ_Level)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(name = 'Difference in Hourly Earnings', labels = dollar) +
  xlab(NULL) + theme(legend.title = element_blank())

# Difference of hourly earnings decreases for all education levels over time,
# especially the high educated. Important to remember this is NOT holding other
# variables fixed

# Average hourly earnings for married vs single
df %>%
  select(YEAR, MONTH, HRLYEARN, married) %>%
  group_by(YEAR, MONTH) %>%
  summarize(Married = mean(HRLYEARN[married == 1]),
            Single = mean(HRLYEARN[married == 0])) %>%
  gather(Mar_Stat, Hourly_Earnings, Married, Single) %>%
  ggplot(aes(YEAR, Hourly_Earnings, color = Mar_Stat)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(name = 'Hourly Earnings', labels = dollar) +
  xlab(NULL) + theme(legend.title = element_blank())

# On average, married earn more than single

# Average hourly earnings for male vs female
df %>%
  select(YEAR, MONTH, HRLYEARN, male) %>%
  group_by(YEAR, MONTH) %>%
  summarize(Male = mean(HRLYEARN[male == 1]),
            Female = mean(HRLYEARN[male == 0])) %>%
  gather(gender, hourly_earnings, Male, Female) %>%
  ggplot(aes(YEAR, hourly_earnings, color = gender)) +
  geom_smooth(se = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_continuous(name = 'Hourly Earnings', labels = dollar) +
  xlab(NULL) + theme(legend.title = element_blank())
