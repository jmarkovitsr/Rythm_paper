# Final projecct 
library(tidyverse)
library(knitr)

# import data
df1 = read_csv('data/vowel_data.csv')
df2 = read_csv('data/Qp_Jenny_doc2.csv')

#------- Prepare df1 --------#
# creat a clean participant id for df1: L#(SPAN|HEB)#
# type of language, 1 or 2
df1 = df1 %>%
  mutate(type_lan = str_extract(language, '[0-9]'))

# language2: SPAN or HEB
df1 = df1 %>%
  mutate(language2 = ifelse(language == 'span1' | language == 'span2', 'SPAN', 'HEB'))

# number individual
df1 = df1 %>%
  mutate(number_id = str_extract(id, '[0-9]{2}$'))

df1 = df1 %>%
  mutate(number_id = str_c('0', number_id)) # a 0

# finally, creat participant 
df1 = df1 %>%
  mutate(PARTICIPANT = str_c('L', type_lan, language2, number_id)) #%>%
  #select(-type_lan, -language2, -number_id)                      # remove variables that not need


#------- Prepare df2 ----------#
# select variables and remove duplicates
df2 = df2 %>%
  select(PARTICIPANT, MINT) %>%
  distinct(PARTICIPANT, .keep_all= TRUE)

#----- Join df1 with df2 using PARTICIPANT id -----#
df1 = left_join(df1, df2, by = "PARTICIPANT")

summary(df1$MINT) # some missing values


# imputation
df1[df1$PARTICIPANT == 'L2SPAN015', 7] = 26

summary(df1$MINT)  # checking missing 


#---- Missing values ----#
# remove missing values (heritage)
df1 = df1 %>%
        na.omit()

summary(df1$MINT)  # checking missing 


# Estadistica

# recode type_lan
df1 = df1 %>%
  mutate(type_lan = recode(type_lan, '1' = 'L1', '2' = 'L2'))

# table
# duration vowel 1
df1 %>%
  group_by(language2, type_lan) %>%
  summarise(mean_v1 = mean(v1_dur), sd_v1 = sd(v1_dur))

#kable(table1, caption = "")

# duration vowel 2
df1 %>%
  group_by(language2, type_lan) %>%
  summarise(mean_v2 = mean(v2_dur), sd_v2 = sd(v2_dur))


# plots
# duration vowel 1
# note: Boxplot include the median (not mean)
# mean: is the sum of total values divided by total number of observation 
# median: oder values, and select the central values
df1 %>%
  ggplot(aes(x = language2, y = v1_dur, fill = type_lan)) +
  geom_boxplot(outlier.colour = "white", outlier.shape = 1, alpha = 0.7) +
  theme_classic() +
  xlab('language') +
  ylab('vowel 1 duration')

# duration vowel 2
# note: Boxplot include the median (not mean)

df1 %>%
  ggplot(aes(x = language2, y = v2_dur, fill = type_lan)) +
  geom_boxplot(outlier.colour = "white", outlier.shape = 1, alpha = 0.7) +
  theme_classic() +
  xlab('language') +
  ylab('vowel 2 duration') +
  ylim(c(0,0.2))


# duration vowel 1 and Mint
# note: mint is not relevant to explain the variability of v1_dur, because low and high values of Mint give
# you the same variability of v1_dur
df1 %>%
  ggplot(aes(x = MINT, y = v1_dur)) +
  geom_point()

# duration vowel 2 and Mint
df1 %>%
  ggplot(aes(x = MINT, y = v2_dur)) +
  geom_point()

# duration vowel 1 and Mint, type_lan
df1 %>%
  ggplot(aes(x = MINT, y = v1_dur, color = type_lan)) +
  geom_point()

# duration vowel 2 and Mint, type_lan
df1 %>%
  ggplot(aes(x = MINT, y = v2_dur, color = type_lan)) +
  geom_point()

# duration vowel 1 and Mint, language2
df1 %>%
  ggplot(aes(x = MINT, y = v1_dur, color = language2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~language2)


# duration vowel 2 and Mint, language2
df1 %>%
  ggplot(aes(x = MINT, y = v2_dur, color = language2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~language2)


#--------------- Corr ----------------#
# Cuando hay mas de una variable independiente, hay que hacer el analisis de correlacion (multicolinealidad)
# dummy of type lan

df1 = df1 %>%
      mutate(type_lan_d = ifelse(type_lan == 'L2', 1, 0))

cor(df1$MINT, df1$type_lan_d)  # tooo high correlation, should be between -0.4 and 0.4

#---------------- duration vowel vs type of language --------------------
# Model1-Heb: y = vowel 1
#         x = type_lan
lm1 = df1 %>%
        filter(language2 == 'HEB') %>%
        lm(v1_dur ~ type_lan_d, data = .) # type_lan_d = 1 if L2, = 0 if L1

summary(lm1)

# Model2-Span: y = vowel 1
#         x = type_lan
lm2 = df1 %>%
  filter(language2 == 'SPAN') %>%
  lm(v1_dur ~ type_lan_d, data = .)

summary(lm2)        
# note of result
# parameter type_lan_d = 0.015951, duration vowel L2 is grater than duration vowel L1 



# Model3-Heb: y = vowel 2
#         x = type_lan

lm3 = df1 %>%
  filter(language2 == 'HEB') %>%
  lm(v2_dur ~ type_lan_d, data = .)

summary(lm3)

# Model4-Span: y = vowel 2
#         x = type_lan

lm4 = df1 %>%
  filter(language2 == 'SPAN') %>%
  lm(v2_dur ~ type_lan_d, data = .)

summary(lm4)

#---------------- duration vowel vs MINT  --------------------
# Model5-Heb: y = vowel 1
#         x = MINT

lm5 = df1 %>%
  filter(language2 == 'HEB') %>%
  lm(v1_dur ~ MINT, data = .) # type_lan_d = 1 if L2, = 0 if L1

summary(lm5)

# Model6-Span: y = vowel 1
#         x = MINT
lm6 = df1 %>%
  filter(language2 == 'SPAN') %>%
  lm(v1_dur ~ MINT, data = .)

summary(lm6)


# Model7-Heb: y = vowel 2
#         x = MINT
lm7 = df1 %>%
  filter(language2 == 'HEB') %>%
  lm(v2_dur ~ MINT, data = .)

summary(lm7)

# Model8-Span: y = vowel 2
#         x = MINT

lm8 = df1 %>%
  filter(language2 == 'SPAN') %>%
  lm(v2_dur ~ MINT, data = .)

summary(lm8)


