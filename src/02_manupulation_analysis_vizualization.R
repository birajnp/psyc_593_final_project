library(here) # for directory
source(here("src", "01_read_clean_merge_data.R"))

#clearing the previous environment and loading packages
rm(list=ls())
# This should be unnecessary

packages <- c("gtsummary","foreign","survey",'labelled',"broom", "tidyverse",
              "haven","rockchalk", "forcats", "data.table", "Hmisc", "srvyr","kableExtra")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

#load libraries
invisible(lapply(packages, require, character.only=TRUE))

#read clean dataset for analysis
df <- readRDS(here('data', 'clean_dataset', 'clean_dataset_for_analysis.rds'))


#Manupulating Variables for Analysis
#lowercasing all the row names in df
names(df) <- tolower(names(df))


#categorization of age in new variable age_category
df <- mutate(df,
             age_category = case_when(
               q1 == '11 years old or younger' ~ '11 years old or younger',
               q1 %in% c("12 years old", "13 years old", "14 years old") ~ "12 to 14 years",
               q1 %in% c("15 years old", "16 years old", '17 years old') ~ "15 to 17 years",
               q1 == '18 years old or older' ~ '18 years old or older',
               TRUE ~ NA_character_
             ))

#' categorization of data female 0 and Male 1 in sex_category variable

# df <- mutate(df,
#              sex_category = ifelse(q2 == "Male", '1',
#                                    ifelse(q2 == "Female",'0', q2)))


# Felt lonely binary
df <- mutate(df,
             felt_lonely = case_when(
               q22 %in% c("Never", "Rarely") ~ "0",
               q22 %in% c("Sometimes", "Most of the time", "Always") ~ "1",
               TRUE ~ q22
             ))





# couldn't sleep q23 bianary

df <- mutate(df,
             couldnot_sleep = case_when(
               q23 %in% c("Never", "Rarely") ~ "0",
               q23 %in% c("Sometimes", "Most of the time", "Always") ~ "1",
               TRUE ~ q23))



# q24 considered suicide
df <- mutate(df,
             considered_suicide = case_when(
               q24 == "No" ~ "0",
               q24 == "Yes" ~ "1",
               TRUE ~ q24))






# q25 made suicide plan

df <- mutate(df,
             made_suicide_pan = ifelse(q25 == "N0" , "0",
                                       ifelse(q25 == "Yes", "0", q25)))
# Both Yes and No are recoded to 0...







# q26 attempted suicide


# Several of the cases of recoding  you have below
# could be simpler using a parse_number() call to extract the values and then
# just a reclasification using 0 or > 0
# 
# Also, you have used case_when in some of the recodings
# That alternative is preferable to the many nested ifelse that you have in many cases below
# Lastly, with som much nesting, it is better to use the two spaces convention of the tidyverse style
# instead of the alignment to the opening parenthesis
# 
df <- mutate(df, 
             attempted_suicide = ifelse(q26 == "0 times" , "0",
                                        ifelse(q26 == "1 time", "1",
                                               ifelse(q26 == "2 or 3 times", "1",
                                                      ifelse(q26 == "4 or 5 times", 1,
                                                             ifelse(q26 == "6 or more times", "1", q26))))))


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

# Smoking frequency
df <- mutate(df,
             smoking_frequency = case_when(
               q29 == "0 days" ~ "Never",
               q29 %in% c("1 or 2 days", "3 to 5 days", "6 to 9 days") ~ "Sometimes",
               q29 == "10 to 19 days" ~ "Most of the times",
               q29 %in% c("20 to 29 days", "All 30 days") ~ "Everyday",
               TRUE ~ q29
             ))


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


# 
# df <- mutate(df,
#              condom_use = ifelse(q47 == "No" , "0",
#                                  ifelse(q47 == "Yes", "1",
#                                         ifelse(q47 == "Never had sex", "Never had sex", q47))))




#'Parents_understand_problems' Q56
df <- mutate(df,
             parent_understand_problems = ifelse(q56 == "Never", "0",
                                                 ifelse(q56 == "Rarely", "0",
                                                        ifelse(q56 == "Sometimes", "0",
                                                               ifelse(q56 == "Most of the time", "1",
                                                                      ifelse(q56 == "Always", "1", q56 ))))))




# parents were aware of free time activities
df <- df %>%
  mutate(q57 = case_when(
    q57 %in% c("Never", "Rarely", "Sometimes") ~ "0",
    q57 %in% c("Most of the time", "Always") ~ "1"
  ))


# Parent go throught things

# df <- df %>%
#    mutate(
#     q58 = recode(q58,
#                    `Never` = 0,
#                    `Rarely` = 1,
#                    `Rarely` = 2,
#                    `Rarely` = 1,
#                    `Mostly` = 3,
#                    `Mostly` = 4,
#                    `Mostly` = 5))



# other students kind and helpful

# df <- df%>%
#   mutate(q54 = recode(q54,
#                       `Never` = '0',
#                       `Rarely` = '0',
#                       `Sometimes` = '0',
#                       `Most of the time` = '1',
#                       `Always` = '1'))


# currently smoked cigarettes QN29 (think of Q29 as well)

# df <- df%>% mutate(smoking_yes_no = case_when(q29,
#                `0 days` = 0,
#                `1 or 2 days` = 1,
#                `3 to 5 days` = 1,
#                `6 to 9 days` = 1,
#                `10 to 19 days` = 1,
#                `20 to 29 days`= 1))

# Recode the Bullied variable
df <- df %>%
  mutate(q20 = case_when(
    q20 == "0 days" ~ "No days",
    q20 %in% c("1 or 2 days", "3 to 5 days", "6 to 9 days") ~ "1-9 days",
    q20 %in% c("10 to 19 days", "20 to 29 days", "All 30 days") ~ "10 or more days"
  ))


# # Anxiety recode
# df <- df %>%
#   mutate(
#     Anxiety = recode(Anxiety, 
#                      `1` = "Most of the times/always", 
#                      `0` = "Never/rarely/sometimes", 
#                      .default = NA_character_)
#   )



#recode physical fighting

df <- mutate(df,
             physical_fight_binary = case_when(q16 == "0 times" ~ "0",
                                               q16 %in% c("1 time", "2 or 3 times", 
                                                          "4 or 5 times", "6 or 7 times",
                                                          "8 or 9 times","10 or 11 times",
                                                          "12 or more times") ~ "1",
                                               TRUE ~ q16))
df$miss_school_cat <- factor(case_when(
  df$q53 == "0 days" ~ "Never",
  df$q53 %in% c("1 or 2 days", "3 to 5 days") ~ "Sometimes",
  df$q53 %in% c("6 to 9 days", "10 or more days") ~ "Often"
))




# education code to actual grade
df <-  mutate(df,
              education = case_when(
                q3 %in% c("M 3", "Class 7")  ~ "Grade 7",
                q3 %in% c("M 4", "Class 8") ~ "Grade 8",
                q3 %in% c("M 5", "Class 9") ~ "Grade 9",
                q3 %in% c("M 6","Class 10") ~ "Grade 10",
                q3 %in% c("M 7","Class 11") ~ "Grade 11",
                q3 == "6" ~ "Grade 6", TRUE ~ q3 ))

# Create new education categories
df$education_cat <- factor(case_when(
  df$education %in% c("Grade 6", "Grade 7", "Grade 8") ~ "Lower Secondary",
  df$education %in% c("Grade 9", "Grade 10") ~ "Middle Secondary",
  df$education %in% c("Grade 11", "Grade 12", "Grade 13") ~ "Higher Secondary"
))


# For seriously injured (q17)
df <- mutate(df,
             injury_cat = case_when(
               q17 == "0 times" ~ "Never",
               q17 %in% c("1 time", "2 or 3 times") ~ "1-3 times",
               q17 %in% c("4 or 5 times", "6 or 7 times") ~ "4-7 times",
               q17 %in% c("8 or 9 times", "10 or 11 times", "12 or more times") ~ "8+ times",
               TRUE ~ q17))

# For bullying (q20)
df <- mutate(df,
             bullying_cat = case_when(
               q20 == "0 days" ~ "Never",
               q20 %in% c("1 or 2 days", "3 to 5 days") ~ "Sometimes",
               q20 %in% c("6 to 9 days", "10 to 19 days", "20 to 29 days", "All 30 days") ~ "Often",
               TRUE ~ q20))

# For hunger (q6)
df <- mutate(df,
             hunger_cat = case_when(
               q6 == "Never" ~ "Never",
               q6 %in% c("Rarely", "Sometimes") ~ "Sometimes",
               q6 %in% c("Most of the time", "Always") ~ "Often",
               TRUE ~ q6))

# drug initiation
df <- mutate(df,
             drug_initiation_cat = case_when(
               q40 %in% c("7 years old or younger", "8 or 9 years old", "10 or 11 years old", "12 or 13 years old") ~ " <13 years",
               q40 %in% c("14 or 15 years old", "16 or 17 years old", "18 years old or older") ~  "13-18 years",
               q40 == "I have never used drugs" ~ "Never used",
               TRUE ~ q40))


df$substance_use <- with(df, {
  as.numeric(as.character(current_marijuna_use)) + 
    as.numeric(as.character(smoking_status)) +
    as.numeric(as.character(other_tobacco_use))
}) #0 = no substance use
#1 = uses one substance (marijuana, smoking, or tobacco)
#2 = uses two substances
#3 = uses all three substances
# Convert injury frequency to binary (any injury vs no injury)
df$any_injury <- ifelse(df$q17 == "0 times", 0, 1)

# Convert serious injury types to binary (any serious injury vs none)
df$serious_injury <- ifelse(df$q18 == "Not seriously injured", 0, 1)

# Approach 2: Categorical Groups
# Group injury frequency into low/medium/high
df$injury_frequency_group <- case_when(
  df$q17 == "0 times" ~ "No injury",
  df$q17 %in% c("1 time", "2 or 3 times") ~ "Low frequency", 
  df$q17 %in% c("4 or 5 times", "6 or 7 times") ~ "Medium frequency",
  TRUE ~ "High frequency"
)

# Create specific injury type indicators
df$bone_injury <- ifelse(df$q18 == "Broken bone/dislocated joint", 1, 0)
df$cut_injury <- ifelse(df$q18 == "I had a cut or stab wound", 1, 0) 
df$head_injury <- ifelse(df$q18 == "Concussion/head injury", 1, 0)

# Create severity indicator based on injury type
df$severe_injury <- ifelse(df$q18 %in% c("Concussion/head injury", "I had a gunshot wound"), 1, 0)


# Group q40 (drug initiation age) with age ranges and never used as baseline
df$drug_init_age <- case_when(
  df$q40 == "I have never used drugs" ~ "Never used",
  df$q40 %in% c("7 years old or younger", "8 or 9 years old") ~ "≤9 years",
  df$q40 %in% c("10 or 11 years old", "12 or 13 years old") ~ "10-13 years",
  df$q40 %in% c("14 or 15 years old", "16 or 17 years old") ~ "14-17 years",
  df$q40 == "18 years old or older" ~ "≥18 years"
)


#setting a data frame
df <- as.data.frame(lapply(df, as.factor))

# converting the data type as numberic

df <- df %>% 
  mutate (
    weight = as.numeric(as.character(weight)),
    psu = as.numeric(as.character(psu)),
    stratum = as.numeric(as.character(stratum))
  )

# Create the survey design object 
svy_dataset <- svydesign(
  data = df,                      
  ids = ~ psu,                       # Primary Sampling Units variable
  strata = ~ stratum,                # Stratum variable
  weights = ~ weight, nest = TRUE                # Weights variable
)


# frequency table
svy_freqs <- function(cat_vars, design) {
  result_list <- list()
  
  for(var in cat_vars) {
    f <- as.formula(paste0("~factor(", var, ", exclude=NULL)"))
    
    # Get counts and percentages
    counts <- svytable(f, design=design)
    pct <- prop.table(counts) * 100
    
    # Create data frame with rounded numbers
    df <- data.frame(
      variable = var,
      category = names(counts),
      n = ceiling(as.numeric(counts)),  # rounds up to nearest whole number
      percent = round(as.numeric(pct), 2)  # rounds to 2 decimal places
    )
    
    result_list[[var]] <- df
  }
  
  # Combine all results
  final_table <- do.call(rbind, result_list)
  row.names(final_table) <- NULL
  
  return(final_table)
}

# cat_vars defining 

cat_vars <- c("country","q2", "age_category", "education_cat","substance_use",
              "drug_initiation_cat", "hunger_cat", "miss_school_cat",
              "felt_lonely",  "q24",# "q17",
              "q18",
              "physical_fight_binary", "q53", "q54", "q20", "q27")

tab1 <- svy_freqs(cat_vars, svy_dataset)


readr::write_csv(tab1, here::here("results", "tables", "frequencies.csv"))
print(tab1)



# cont_summary <- svy_summary(cont_vars, survey)
svy_dataset$variables$q4 <-  as.numeric(as.character(svy_dataset$variables$q4))
svy_dataset$variables$q5 <-  as.numeric(as.character(svy_dataset$variables$q5))

svy_summary <- function(cont_vars, design) {
  result_list <- lapply(cont_vars, function(x) {
    # Get summary for this variable including missing
    tab <- svytable(as.formula(paste0("~factor(", x, ", exclude=NULL)")), design=design)
    
    # Calculate mean and sd for non-missing values
    mean_val <- svymean(as.formula(paste0("~", x)), design=design, na.rm=TRUE)
    sd_val <- sqrt(svyvar(as.formula(paste0("~", x)), design=design, na.rm=TRUE))
    
    # Extract missing count specifically and handle NA in names
    missing_count <- sum(tab[is.na(names(tab))])
    
    df <- data.frame(
      variable = x,
      n = ceiling(sum(tab)),
      n_missing = ceiling(missing_count),
      mean = round(as.numeric(mean_val), 2),
      sd = round(as.numeric(sd_val), 2),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
    df
  })
  
  do.call(rbind, result_list)
}

#define varaibales
cont_vars <- c("q4", "q5")

cont_summary <- svy_summary(cont_vars, design = svy_dataset)
# table 1 summary
print(cont_summary)
readr::write_csv(cont_summary, here("results", "tables", "cont_var_sum.csv"))

#-------------formatted table ===============================================
# First modify the data to replace <NA> with "Missing"
tab1_modified <- tab1 %>%
  mutate(category = case_when(
    is.na(category) ~ "Missing",
    category == "<NA>" ~ "Missing",
    category == "NA" ~ "Missing",
    TRUE ~ category
  )) %>%
  mutate(variable = "") 

# Create continuous variable rows with exact statistics
cont_rows <- data.frame(
  variable = "",
  category = c(
    sprintf("Mean (SD) = %.2f (%.2f)", 1.55, 0.10),  # q4
    sprintf("Missing"),
    sprintf("Mean (SD) = %.2f (%.2f)", 45.80, 10.49), # q5
    sprintf("Missing")
  ),
  n = c(
    9105754,    # n for q4
    1548634,    # n_missing for q4
    9105754,    # n for q5
    1548634     # n_missing for q5
  ),
  percent = c(
    NA,
    14.52,      # Percentage missing (1548634/10654388*100)
    NA,
    14.52       # Same missing percentage
  )
)

# Calculate total N
total_n <- sum(tab1$n[1:5])

# Create total row
total_row <- data.frame(
  variable = "",
  category = "Total",
  n = total_n,
  percent = 100.00
)

# Combine total row
tab1_final <- bind_rows(total_row, tab1_modified, cont_rows)

# Create the formatted table
tab1_final %>%
  kbl(col.names = c("Variable", "Category", "N", "Percentage"),
      align = c("l", "l", "r", "r"),
      format.args = list(big.mark = ",", decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "left") %>%
  row_spec(1, bold = TRUE) %>%
  pack_rows("Country", 2, 6) %>%
  pack_rows("Gender", 7, 9) %>%
  pack_rows("Age Group", 10, 14) %>%
  pack_rows("Education Level", 15, 18) %>%
  pack_rows("Substance Use", 19, 23) %>%
  pack_rows("Drug Initiation Age", 24, 27) %>%
  pack_rows("Experience of Hunger", 28, 31) %>%
  pack_rows("School Absence", 32, 35) %>%
  pack_rows("Felt Lonely", 36, 38) %>%
  pack_rows("Considered Suicide", 39, 41) %>%
  pack_rows("Injury Type", 42, 50) %>%
  pack_rows("Engaged in Physical Fighting", 51, 53) %>%
  pack_rows("Days Missing School", 54, 59) %>%
  pack_rows("Parent Understanding", 60, 65) %>%
  pack_rows("Bullied", 66, 69) %>%
  pack_rows("Close Friends", 70, 74) %>%
  pack_rows("Height (meters)", 75, 76) %>%
  pack_rows("Weight (kg)", 77, 78) %>%
  column_spec(2, width = "15em") %>%
  column_spec(3:4, width = "8em")



#======================================= logistic model for our analysis
library(car)
svy_dataset$variables$age_category <- relevel(svy_dataset$variables$age_category, ref = "11 years old or younger")
svy_dataset$variables$q20 <- relevel(svy_dataset$variables$q20, ref = "No days")
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


library(sjPlot)
tab_model(model)
summary(model)

vif(model)





##============================================================================
#clean data for visualization 
df2 <- na.omit(df)

#  BMI data
df2$q4 <- as.numeric(trimws(df2$q4))  
df2$q5 <- as.numeric(as.character(df2$q5))  
df2$bmi <- df2$q5 / (df2$q4^2)

# BMI visualization
bmi_plot <- ggplot() +
  theme_minimal() +
  geom_jitter(
    data = filter(df2, substance_use != "0"),
    aes(x = substance_use, y = bmi, color = q2, shape = education_cat),
    alpha = 0.6, width = 0.2, size = 2
  ) +
  geom_point(
    data = df2 %>%  # Changed from df to df2
      filter(substance_use != "0") %>%
      group_by(country, q2, substance_use) %>%
      summarise(mean_bmi = mean(bmi, na.rm = TRUE), .groups = "drop"),
    aes(x = substance_use, y = mean_bmi, color = q2),
    size = 5, shape = "diamond"
  ) +
  facet_wrap(~country, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(
    name = "Gender",
    labels = c("Male", "Female")
  ) +
  scale_shape_manual(
    name = "Education Level",
    values = c(16, 17, 18),
    labels = c("Higher Secondary", "Lower Secondary", "Middle Secondary")
  ) +
  scale_x_discrete(
    labels = c("Single substance", "Two substances", "Three Substances")
  ) +
  labs(
    title = "BMI Distribution Among Substance Users",
    subtitle = "By Number of Substances Used, Gender, and Education Level",
    x = "Number of Substances Used",
    y = "Body Mass Index (BMI)",
    caption = "Diamond shapes indicate mean BMI values"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )


print(bmi_plot)

ggsave(filename = here("results", "figures", "bmi_plot.jpeg"), plot = bmi_plot)



# alluvial plot
alluvial_data <- df2 %>%
  mutate(
    fight = factor(physical_fight_binary, labels = c("Fight", "No fight")),
    substance = factor(substance_use, labels = c("No Use", "One", "Two", "Three")),
    parent_understand = factor(parent_understand_problems,
                               labels = c("Don't\nUnderstand", "Understand")),
    bullying = factor(bullying_cat, 
                      labels = c("1-9\ndays", "10+\ndays", "No\nBullying")),
    hunger = factor(hunger_cat, 
                    labels = c("Never", "Often", "Sometimes"))
  ) %>%
  group_by(substance, parent_understand, bullying, hunger, fight) %>%
  summarise(
    count = n(),
    pct = n() / nrow(df) * 100,
    .groups = "drop"
  ) %>% 
  select(!pct) 


library(ggalluvial)
alluvial_data %>% data_to_alluvium()

alluvial_plot <- ggplot(alluvial_data,
                        aes(y = count,
                            axis1 = substance,
                            axis2 = parent_understand,
                            axis3 = bullying,
                            axis4 = hunger)) +#,
  # axis5 = fight)) +
  geom_alluvium(aes(fill = substance),  
                alpha = 0.8, 
                stat = "alluvium",
                width = 1/3,
                knot.pos = 0.4) +
  geom_stratum(width = 1/4, 
               fill = "white", 
               color = "darkgrey",
               alpha = 0.9) +
  geom_text(stat = "stratum",
            aes(label = paste0(after_stat(stratum), "\n", 
                               scales::percent(after_stat(prop), accuracy = 0.1))),
            size = 2,    
            fontface = "bold",
            lineheight = 0.8) +
  scale_y_log10() +
  scale_fill_viridis_d(option = "magma", 
                       begin = 0.2, 
                       end = 0.9) +
  labs(title = "Substance Use and Risk Factors",
       subtitle = "Examining relationships between key risk factors (Log Scale)",
       fill = "Substance Use") +
  # annotate("text", x = 1:5, y = rep(0.00001, 5),
  annotate("text", x = 1:4, y = rep(0.00001, 4),
           label = c("Substance\nUse", "Parental\nUnderstanding", 
                     "Bullying\nExperience", "Hunger\nFrequency"),
           # "Physical\nFight"),  
           fontface = "bold", size = 3, lineheight = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, color = "grey30", margin = margin(b = 20)),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box.margin = margin(t = 20),
    plot.margin = margin(t = 30, r = 30, b = 30, l = 30)
  )


# ggsave(filename = here("results", "figures", "alluvial_plot.jpeg"), plot = alluvial_plot)

print(alluvial_plot)


# model forest plot
# Tidy up the model results 
model_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

# forest plot of odds ratios (OR) with 95% CI
model_plot <- ggplot(model_results, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +   # Point range (OR + CI)
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # def reference line at OR = 1
  coord_flip() +  # Flip axes for better readability
  theme_minimal() + # Use a minimal theme
  labs(
    title = "Survey-Weighted Logistic Regression Plot: Odds Ratios and 95% Confidence Intervals",
    x = "Predictors",
    y = "Odds Ratio (OR)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

print(model_plot)

ggsave(filename = here("results", "figures", "model_plot.jpeg"), plot = model_plot, width = 10,                 # Width of the image (in inches)
       height = 8,  
       dpi = 300, )



