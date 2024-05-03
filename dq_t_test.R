library(tidyverse)
# DQ Analysis -------------------------------------------------------------
dq_df <- read.csv("C:/Users/valen/OneDrive/Desktop/dq_all_shift_cap.csv")
# Counts of recorded sizes: Size small was ordered the most
table(dq_df$Size)
dq_df %>% select(1, 2) %>% group_by(Size) %>% summarise(avg_weight = mean(Weight),
                                                        std = sd(Weight),
                                                        n = n())
# Distribution of small cones. Looks apporximately symmetric with few outliers, sample size is greater than 30
# Sample is obtained from collecting data from one week out of the year. sample is not random. However, I beleive the sample
# is representative of typical production conditions. Person making cones was determined randomly
# Independence is a slight issue, but cone-makers were rotated and taught consistent technique
# to make cones in a standardized manner.
dq_df %>% filter(Size == "Small") %>%
ggplot(mapping = aes(x = Weight)) +
  geom_histogram(binwidth = .1)

# t-test for small cones ------------------------------------------------------------------
# Small cones are 5oz
# Ho:mu is equal to 5
# Ha:mu is not equal to 5
# significance level is equal to .05
# t = (x - mu)/(sd/sqrt(n))
small_df <- dq_df %>% filter(Size == "Small")
se <- sd(small_df$Weight)/sqrt(390)
# t stat of -13.52
t <- (mean(small_df$Weight)-5)/se

# double checking my work
t.test(small_df$Weight, mu = 5, alternative = "two.sided", conf.level = .95)
# We reject the null hypothesis. There is significant evidence to conclude that the mean weight for cones differs from 5 oz
# There is sufficient evidence to suggest that customers do not receive 5oz of ice-cream when they order small cones.

# Medium Cones ------------------------------------------------------------
# Same assumptions about small cones
# Distribution is not normal, sample is not random, and fewer observations.
medium_df <- dq_df %>% filter(Size == "Medium")
# Right tailed
hist(medium_df$Weight)
mean(medium_df$Weight)
# Median is 7.1
median(medium_df$Weight)

# Large Cones
large_df <- dq_df %>% filter(Size == "Large")
hist(large_df$Weight)
mean(large_df$Weight)

# EDA ---------------------------------------------------------------------
# Distribution of cone sizes produced during each shift
# Proportion of small cones is greater in the morning than night.
# Morning shift: 11:00 - 17:00
# Night shift: 17:00 - 22:00
dq_df %>% 
  ggplot() +
  geom_bar(mapping = aes(x = Shift, fill = Size))

# Comparison of distribution among workers
dq_df %>% ggplot(mapping = aes(x = Name, y = Weight)) +
  geom_boxplot() +
  facet_wrap(~ Size, nrow = 2) + theme_minimal()



