library(here)
source(here("codes", "02_variable_selection_and_data_merge.R"))
names(df) <- tolower(names(df))

#categorization of age in new variable age_category

df <- mutate(df,
             age_category = case_when(
                  q1 == '11 years old or younger' ~ '11 years old or younger',
                  q1 %in% c("12 years old", "13 years old", "14 years old") ~ "12 to 14 years",
                  q1 %in% c("15 years old", "16 years old", '17 years old') ~ "15 to 17 years",
                  q1 == '18 years old or older' ~ '18 years old or older',
                    TRUE ~ NA_character_))

#' categorization of data female 0 and Male 1 in sex_category variable

df <- mutate(df,
             sex_category = ifelse(q2 == "Male", '1',
                                   ifelse(q2 == "Female",'0', q2)))

#Hunger (proxy for socioeconomic status)	
#‘During the past 30 days, how often did you go hungry because there was not 
# #enough food in your home?’	‘1 = never to 5 = always (coded 1–3 = 0 and 4–5 = 1)’
# df <- mutate(df,
#              birthcontrol_use = ifelse(q48 == "No" , 0,
#                                  ifelse(q48 == "Yes", 1,
#                                     ifelse(q48 == "Never had sex", "Never had sex",
#                                       ifelse(q48 == "I do not know",0 , q48)))))



#outcome variables


#outcome variables
table(df$q16)
table(df$q17)



#features 
# felt lonely q22 bianary

df <- mutate(df,
             felt_lonely = ifelse(q22 == "Never" , "0",
                            ifelse(q22 == "Rarely", "0",
                              ifelse(q22 == "Sometimes", "1",
                                ifelse(q22 == "Most of the time", "1",
                                  ifelse(q22 == "Always", "1", q22))))))
# couldn't sleep q23 bianary
df <- mutate(df,
             couldnot_sleep = ifelse(q23 == "Never" , "0",
                                  ifelse(q23 == "Rarely", "0",
                                         ifelse(q23 == "Sometimes", "1",
                                                ifelse(q23 == "Most of the time", "1",
                                                       ifelse(q23 == "Always", "1", q23))))))
# q24 considered suicide

df <- mutate(df,
             considered_suicide = ifelse(q24 == "N0" , "0",
                                  ifelse(q24 == "Yes", "0", q24)))

# q25 made suicide plan

df <- mutate(df,
             made_suicide_pan = ifelse(q25 == "N0" , "0",
                                         ifelse(q25 == "Yes", "0", q25)))

# q26 attempted suicide

df <- mutate(df,
             attempted_suicide = ifelse(q26 == "0 times" , "0",
                                         ifelse(q26 == "1 time", "1",
                                         ifelse(q26 == "2 or 3 times", "1",
                                         ifelse(q26 == "4 or 5 times", 1,
                                         ifelse(q26 == "6 or more times", "1", q26))))))

table(df$attempted_suicide)
table(df$q26)


# participants current smoking status q29 No = 0 and Yes = 1
# question smoked in last 30 days
df <- mutate(df,
             smoking_status = ifelse(q29 == "0 days" , 0,
                                ifelse(q29 == "1 or 2 days", 1,
                                  ifelse(q29 == "3 to 5 days", 1,
                                    ifelse(q29 == "6 to 9 days", 1,
                                      ifelse(q29 == "10 to 19 days", 1,
                                       ifelse(q29 == "20 to 29 days", 1,
                                        ifelse(q29 == "All 30 days", 1, q29)))))))) 



# smoking frequency o = Never, 1-9 days = sometimes, 
 
df <- mutate(df,
  smoking_frequency = ifelse(q29 == "0 days" , "Never",
                      ifelse(q29 == "1 or 2 days", 'Sometimes',
                      ifelse(q29 == "3 to 5 days", 'Sometimes',
                      ifelse(q29 == "6 to 9 days", 'Sometimes',
                      ifelse(q29 == "10 to 19 days", 'Most of the times',
                      ifelse(q29 == "20 to 29 days", 'Everyday',
                      ifelse(q29 == "All 30 days", 'Everyday', q29)))))))) 



table(df$smoking_status)
table(df$q29)



# question other tobacco use in last 30 days
df <- mutate(df,
             other_tobacco_use = ifelse(q30 == "0 days" , 0,
                                 ifelse(q30 == "1 or 2 days", 1,
                                 ifelse(q30 == "3 to 5 days", 1,
                                 ifelse(q30 == "6 to 9 days", 1,
                                 ifelse(q30 == "10 to 19 days", 1,
                                 ifelse(q30 == "20 to 29 days", 1,
                                 ifelse(q30 == "All 30 days", 1, q30))))))))

df <- mutate(df,
    other_tobacco_use_freq =ifelse(q30 == "0 days" , "Never",
                            ifelse(q30 == "1 or 2 days", 'Sometimes',
                            ifelse(q30 == "3 to 5 days", 'Sometimes',
                            ifelse(q30 == "6 to 9 days", 'Sometimes',
                            ifelse(q30 == "10 to 19 days", 'Most of the times',
                            ifelse(q30 == "20 to 29 days", 'Everyday',
                            ifelse(q30 == "All 30 days", 'Everyday', q30)))))))) 

table(df$q30)
table(df$other_tobacco_use)



# question current alcohol use
df <- mutate(df,
             current_alcohol_use = ifelse(q35 == "0 days" , 0,
                                   ifelse(q35 == "1 or 2 days", 1,
                                   ifelse(q35 == "3 to 5 days", 1,
                                   ifelse(q35 == "6 to 9 days", 1,
                                   ifelse(q35 == "10 to 19 days", 1,
                                   ifelse(q35 == "20 to 29 days", 1,
                                   ifelse(q35 == "All 30 days", 1, q30))))))))

summary(df$q35)
table(df$current_alcohol_use)

# current marijuana use yes = 1 no = 0

df <- mutate(df,
             current_marijuna_use = ifelse(q42 == "0 times" , 0,
                                    ifelse(q42== "1 or 2 times", 1,
                                    ifelse(q42 == "3 to 9 times", 1,
                                    ifelse(q42 == "10 to 19 times", 1,
                                    ifelse(q42 == "20 or more times", 1, q42))))))


df <- mutate(df,
             marijuna_use_frequency = ifelse(q29 == "0 times" , "Never",
                                      ifelse(q29 == "1 or 2 times", 'Sometimes',
                                      ifelse(q29 == "3 to 9 times", 'Sometimes',
                                      ifelse(q29 == "10 to 19 times", 'Most of the times',
                                      ifelse(q29 == "10 to 19 days", 'Most of the times',
                                      ifelse(q29 == "20 to 29 days", 'Everyday',
                                      ifelse(q29 == "All 30 days", 'Everyday', q29)))))))) 

summary(df$q42)
table(df$current_marijuna_use)



df2 <-drop_na(df)

table(df2$country)


table(df$attempted_suicide)


table(df$q22)
table(df$q23)
table(df$q24)
table(df$q25)
table(df$q26)





summary(df$q44)
table(df$ever_sexual_intercourse)











#During the past 30 days, how often did your parents or guardians go through your things without your approval?
table(data_bangladesh$q58)
table(data_nepal$q58)
table(data_bhutan$q58)


# 
df <- mutate(df,
             condom_use = ifelse(q47 == "No" , "0",
                                 ifelse(q47 == "Yes", "1",
                                        ifelse(q47 == "Never had sex", "Never had sex", q47))))




#'Parents_understand_problems' Q56
df <- mutate(df,
             parent_understand_problems = ifelse(q56 == "Never", "0",
                                            ifelse(q56 == "Rarely", "0",
                                             ifelse(q56 == "Sometimes", "0",
                                              ifelse(q56 == "Most of the time", "1",
                                               ifelse(q56 == "Always", "1", q56 ))))))
                                                  
           
table(df$q56)


# parents were aware of free time activities

df <- df %>%
  mutate(
    q57 = recode(q57,
                 `Never` = '0',
                 `Rarely` = '0',
                 `Sometimes` = '0',
                 `Most of the time` = '1',
                 `Always` = '1'))

table(df$q57) 


# Parents check homework
# table(df$q58
#       )
# df <- df %>%
#   mutate(
#     q58 = recode(q58,
#                  `Never` = '0',
#                  `Rarely` =
#                 



# other students kind and helpful

table(df$q54)

df <- df%>%
  mutate(q54 = recode(q54,
                      `Never` = '0',
                      `Rarely` = '0',
                      `Sometimes` = '0',
                      `Most of the time` = '1',
                      `Always` = '1'))


# currently smoked cigarettes QN29 (think of Q29 as well)
table(df$q29)

df <- df%>% 
  mutate(
    smoking_yes_no= recode(q29,
               `0 days` = 0,
               `1 or 2 days` = 1,
               `3 to 5 days` = 1,
               `6 to 9 days` = 1,
               `10 to 19 days` = 1,
               `20 to 29 days`= 1))



table(df$smoking_yes_no) # from q29 column


summary(df$qn27)


# recoding qn27 for bhutan data to make it consistant



table(df$qn27)
table(data_bangladesh$qn27)
table(data_bhutan$qn27)







table(df$qn29)

table (df$q28)
table(df$q30)
table(df$q31)
table(df$q33)
table(df$q53)



summary(df$q29)
summary(df$q30)

summary(df$q44)
summary(df$qn44)


summary(data_bangladesh$Q44)
summary(data_nepal$q44)
summary(data_bhutan$Q44)

summary(data_bangladesh$QN46)
summary(data_nepal$qn46)
summary(data_bhutan$QN46)

#other tobacco use

summary(df$q30)



df_variables <- names(df)

print(df_variables)

df <- labelled::copy_labels(np_selected, df)

summary(df$q29)
summary(data_bangladesh$q29)
summary(data_bhutan$q29)
summary(data_nepal$q29)





# Recode the Bullied variable
df <- df %>%
  mutate(
    q20 = recode(q20, 
                     `0` = "0 days", 
                     `1` = "1 or more times"))



# Assuming df is your dataframe and Anxiety is the column to recode
df <- df %>%
  mutate(
    Anxiety = recode(Anxiety, 
                     `1` = "Most of the times/always", 
                     `0` = "Never/rarely/sometimes", 
                     .default = NA_character_)
  )







# droppping the missing value in Q1 AGE variable
summary(df$q1)

#df <- df %>% drop_na()


summary(df$q1)



summary(df$q48)



