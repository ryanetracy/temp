library(rstatix)
library(tidyverse)

df <- read.delim('RExam.dat')

head(df)
str(df)

m1 <- lm(exam ~ uni, data = df)
summary(m1)

df %>%
  ggplot(aes(uni, exam)) +
  geom_point(shape = 4,
             alpha = .8,
             color = 'blue2',
             position = position_jitter(.05, 0)) +
  geom_smooth(method = 'lm', color = 'red') +
  theme_light() +
  scale_x_continuous(breaks = c(0,1))

ggsave('first plot.jpeg', device = 'jpeg', units = 'cm')

