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


library(devtools)

url <- 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'

source_url(url)


# get d values
m1 <- mean(df$exam[df$uni==0])
m2 <- mean(df$exam[df$uni==1])

sd1 <- sd(df$exam[df$uni==0])
sd2 <- sd(df$exam[df$uni==1])

get_cohen_d(m1, m2, sd1, sd2)


m2 <- lm(exam ~ uni * lectures, data = df)
summary(m2)

df$uni <- factor(df$uni)

df %>%
  ggplot(aes(lectures, exam, color = uni)) +
  geom_point(shape = 4,
             alpha = .8,
             position = position_jitter(.05, 0)) +
  geom_smooth(method = 'lm') +
  theme_light() +
  scale_color_manual(values = c('turquoise2', 'black')) 

ggsave('lectures are bad?.jpeg', device = 'jpeg', units = 'cm')
