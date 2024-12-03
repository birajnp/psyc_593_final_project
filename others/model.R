library(car)

# Createcar# Create correlation matrix with all variables
vars_for_cor <- svy_dataset$variables[, c("physical_fight_binary", 
                                          "q2",  # gender
                                          "education_cat",
                                          "couldnot_sleep", # could not sleep
                                          "age_category",
                                          "felt_lonely",
                                          "considered_suicide",
                                          "made_suicide_pan",
                                          "attempted_suicide",
                                          "q20", # bullying
                                          "q22", # felt lonely
                                          "q17", # seriously injured
                                          "smoking_status",
                                          "smoking_frequency",
                                          "other_tobacco_use",
                                          "current_marijuna_use",
                                          "q27", # close friends
                                          "q54", # other students kind
                                          "q56", # parents understand
                                          "q57", # parents know free time
                                          "q33", # parental tobacco
                                          "q29", # current cigarette
                                          "smoking_frequency" ,# other tobacco
                                         "q53", # miss school
                                          "parent_understand_problems",
                                          "q5", "q6", "q28",
                                          "q40", "q58" 
)]

# Convert factors to numeric
vars_numeric <- data.frame(lapply(vars_for_cor, as.numeric))

# Create correlation matrix with correct use parameter
cor_matrix <- cor(vars_numeric, use = "pairwise.complete.obs")



library(corrplot)

print(cor_matrix)

# Create correlation plot
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)

svy_dataset$variables$age_category <- relevel(svy_dataset$variables$age_category, ref = "11 years old or younger")
svy_dataset$variables$q20 <- relevel(svy_dataset$variables$q20, ref = "0 days")
svy_dataset$variables$q17 <- relevel(svy_dataset$variables$q17, ref = "0 times")
svy_dataset$variables$q18 <- relevel(svy_dataset$variables$q18, ref = "Not seriously injured")
svy_dataset$variables$q40 <- relevel(svy_dataset$variables$q40, ref = "I have never used drugs")
svy_dataset$variables$q6 <- relevel(svy_dataset$variables$q6, ref = "Never")
svy_dataset$variables$country <- relevel(svy_dataset$variables$country, ref = "Afghanistan")
svy_dataset$variables$q27 <- relevel(svy_dataset$variables$q27, ref = "0")
svy_dataset$variables$q56 <- relevel(svy_dataset$variables$q56, ref = "Never")
svy_dataset$variables$q53 <- relevel(svy_dataset$variables$q53, ref = "0 days")
svy_dataset$variables$education_cat <- relevel(svy_dataset$variables$education_cat, ref = "Lower Secondary")
svy_dataset$variables$drug_initiation_cat <- relevel(svy_dataset$variables$drug_initiation_cat, ref = "Never used")


table(svy_dataset$variables$q27)

# Model
model <- svyglm(physical_fight_binary ~ 
                  country + 
                  #age_category +
                  q2 + 
                  education_cat +
                  felt_lonely +
                  couldnot_sleep +
                  considered_suicide +
                 bone_injury +
                cut_injury +
                head_injury +
                  substance_use +
                  serious_injury + 
                  drug_initiation_cat 
                +hunger_cat +
                  miss_school_cat,
                family = binomial(),
                design = svy_dataset)

tab_model(model)
summary(model)

vif(model)



model2 <- svyglm( miss_school_cat ~ 
                    physical_fight_binary +country + 
                  age_category +
                  q2 + 
                  education_cat +
                  felt_lonely +
                  couldnot_sleep +
                  considered_suicide +
                  bone_injury +
                  cut_injury +
                  head_injury +
                  substance_use +
                  q54 + q6  + parent_understand_problems,
                family = binomial(),
                design = svy_dataset)



vif(model2)

tab_model(model2)



unique(svy_dataset$variables$country)


tab_model(final_model)


summary(final_model)

vif(final_model)
