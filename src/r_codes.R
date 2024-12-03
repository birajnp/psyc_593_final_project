#Loading required packages for the study
# load packages

packages <- c("gtsummary","foreign","survey",'labelled',"readxl", "tidyverse",
              "haven","rockchalk", "forcats", "data.table", "Hmisc", "srvyr", "here")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
invisible(lapply(packages, require, character.only=T))


#Loading data for every countries
# read data set
# Data Set Afghanistan 2014

data_afghanisthan <- read_dta(here("data","raw_datasets", 
                                   "1_Afghanistan2014.dta"))
data_afghanisthan <- forcats::as_factor(data_afghanisthan, only_labelled = TRUE)

# fwrite(data.table(Column_Name = names(data_afghanisthan), 
#                   Label = sapply(var_label(data_afghanisthan), 
#                                  function(x) ifelse(is.null(x), "", x))),
#       "column_labels_afghanisthan.txt", sep = ";", 
#        col.names = TRUE, 
#        row.names = FALSE)


# Data Set Bangladesh 2014
data_bangladesh <- read_dta(here("data","raw_datasets","2_Bangladesh2014.dta"))
data_bangladesh <- forcats::as_factor(data_bangladesh, only_labelled = TRUE)

# education code to actual grade on bangladesh data
# data_bangladesh <- data_bangladesh %>% mutate (Q3 = case_when( 
#                                               Q3 == "M 3" ~ "Class 7",
#                                                Q3 == "M 4" ~ "Class 8",
#                                                Q3 == "M 5" ~ "Class 9",
#                                                Q3 == "M 6" ~ "Class 10", 
#                                                Q3 == NA ~ NA ))


# # Write the data.table to a file while replacing NULL values 
# fwrite(data.table(Column_Name = names(data_bangladesh), 
#                   Label = sapply(var_label(data_bangladesh), 
#                                  function(x) ifelse(is.null(x), "", x))),
#       "column_labels_afghanisthan.txt", sep = ";", 
#        col.names = TRUE, 
#        row.names = FALSE)


#Data set Bhutan 2016

data_bhutan <- read_dta(here("data", "raw_datasets", "3_Bhutan2016.dta"))
data_bhutan <- forcats::as_factor(data_bhutan, only_labelled = TRUE)

# # Write the data.table to a file while replacing NULL values 
# here(fwrite(data.table(Column_Name = names(data_bhutan), 
#                   Label = sapply(var_label(data_bhutan), 
#                                  function(x) ifelse(is.null(x), "", x))),
#        "column_labels_bhutan.txt", sep = ";", 
#        col.names = TRUE, 
#        row.names = FALSE))


# Data Set Maldives 2014

data_maldives <- read_dta(here("data","raw_datasets", "4_Maldives2014.dta"))
data_maldives <- forcats::as_factor(data_maldives, only_labelled = TRUE)

# Write the data.table to a file while replacing NULL values 
# fwrite(data.table(Column_Name = names(data_maldives), 
#                   Label = sapply(var_label(data_maldives), 
#                                  function(x) ifelse(is.null(x), "", x))),
#       "column_labels_maldives.txt", sep = ";", 
#        col.names = TRUE, 
#        row.names = FALSE)




# Data Set Nepal 2015

data_nepal <- read_dta(here("data","raw_datasets", "5_Nepal2015.dta"))
data_nepal <- forcats::as_factor(data_nepal, only_labelled = TRUE)

# fwrite(data.table(Column_Name = names(data_nepal), 
#                   Label = sapply(var_label(data_nepal), 
#                                  function(x) ifelse(is.null(x), "", x))),
#        "column_labels_nepal.txt", sep = ";",
#        col.names = TRUE, 
#        row.names = FALSE
#        )

# Data Set Srilanka 2016

data_srilanka <- read_dta(here("data","raw_datasets", "6_Srilanka2016.dta"))
data_srilanka <- forcats::as_factor(data_srilanka, only_labelled = TRUE)

# fwrite(data.table(Column_Name = names(data_srilanka), 
#                   Label = sapply(var_label(data_srilanka), 
#                                  function(x) ifelse(is.null(x), "", x))),
#       "column_labels_srilanka.txt", sep = ";", 
#        col.names = TRUE, 
#        row.names = FALSE

#Extracting variable names and their associated labels to understand the data-set
# function to get variable labels (if present) or just names
get_var_label <- function (data, dataset_name) {
  data.frame (
    Column_Name = colnames(data),
    Label = sapply(var_label (data), 
                   function(x) ifelse(is.null(x), "", x)),
    Dataset = dataset_name )}

# Get variable labels from each dataset
afghanisthan_labels <- get_var_label (data_afghanisthan, "Afghanisthan")

bangladesh_labels <- get_var_label (data_bangladesh, "Bangladesh")

bhutan_labels <- get_var_label (data_bhutan, "Bhutan")

maldives_labels <- get_var_label (data_maldives, "Maldives")

nepal_labels <- get_var_label (data_nepal, "Nepal")

srilanka_labels <- get_var_label (data_srilanka, "Srilanka")

# Combine all labels into one data frame
combined_label <- bind_rows(afghanisthan_labels, bangladesh_labels, 
                            bhutan_labels,maldives_labels, nepal_labels, 
                            srilanka_labels )

# Write the combined labels to a CSV file for comparison
#write_csv(combined_label, 'combined_data_labels_raw.csv')


#Procedure to combine data set, from our analysis we saw that the data file doesn't have consistent name so we need to lowercase the variable name across all data set.
# lower casing the variables name

names(data_afghanisthan) <- tolower(names(data_afghanisthan))

names(data_bangladesh) <- tolower(names(data_bangladesh))

names(data_bhutan) <- tolower(names(data_bhutan))

names(data_maldives) <- tolower(names(data_maldives))

names(data_nepal) <- tolower(names(data_nepal))

names(data_srilanka) <- tolower(names(data_srilanka))

##Selecting and defining variable names for our study.
#selecting variables and assigning it to as a cor_vars
core_vars <- c(
  # independent variables
  'q1', 'q2', 'q3',                         # Age, Sex, grade
  'q6', # Hunger questionnaire proxy for socioeconomic status
  'q4', 'q5',  # how tall are you, how much do you weight
  'q17',  #Seriously injured
  'q18', #Serious injury type
  #'q19', #Serious injury cause (missing in bangladesh)
  
  'q20', 'q22', 'q23',     # 'q21' missing in bangladesh)   # bullied, bullied how,  Mental health: Felt lonely, could not sleep
  'q24', 'q25', 'q26', 'q27',  # suicide and other mental health
  'q28', 'q29', 'q30', 'q33',   # Tobacco use: Initiation, current use, cessation, exposure, parental use
  #'q34', 'q35',  # Alcohol use: Initiation, current use, frequency, source, effects (afghanistan missing this variable)
  'q40','q41', 'q42',  #other drugs and marijuana 
  'q56', 'q57', 'q58',        # parent understand problem, know about free time, go through things
  'q53', 'q54', # CLASS ATTENDANCE AND PEER SUPPORT
  
  #outcome variables
  'q16','q17',  #physically attacked      # Physical fighting
  
  #sample weight and related variable
  'weight', 'stratum', 'psu' )                # Sample weights and design variables


# Subset the data sets to include only the selected variables
af_selected <- data_afghanisthan %>% select(all_of(core_vars))

bd_selected <- data_bangladesh %>% select(all_of(core_vars))

bt_selected <- data_bhutan %>% select(all_of(core_vars))

md_selected <- data_maldives %>% select(all_of(core_vars))

np_selected <- data_nepal %>% select(all_of(core_vars))

sl_selected <- data_srilanka %>% select(all_of(core_vars))

# extracting column name for each dataset
af_cols <- names(af_selected) #Afghanisthan

bd_cols <- names(bd_selected) #Bangladesh

bt_cols <- names(bt_selected) #Bhutan

md_cols <- names(md_selected) #Maldives

np_cols <- names(np_selected) #Nepal

sl_cols <- names(sl_selected) #Srilanka

# Checking if column names match across all datasets
cols_match <- cols_match <- all(
  bd_cols == bt_cols & 
    bt_cols == np_cols & 
    np_cols == md_cols & 
    md_cols == af_cols & 
    af_cols == sl_cols
)

cols_match

# required as some data type differed function to convert all columns to factors
convert_all_to_factors <- function(df) {
  df %>%
    mutate(across(everything(), ~ as.factor(.)))
}

# Conversion function to convert all columns to factors for each dataset
af_selected <- convert_all_to_factors(af_selected)  # Afghanistan
bd_selected <- convert_all_to_factors(bd_selected)  # Bangladesh
bt_selected <- convert_all_to_factors(bt_selected)  # Bhutan
md_selected <- convert_all_to_factors(md_selected)  # Maldives
np_selected <- convert_all_to_factors(np_selected)  # Nepal
sl_selected <- convert_all_to_factors(sl_selected)  # Sri Lanka


# Ensure all datasets have the same columns as Nepal
af_selected <- af_selected %>%
  select(all_of(names(np_selected)))


bd_selected <- bd_selected %>%
  select(all_of(names(np_selected)))

bt_selected <- bt_selected %>%
  select(all_of(names(np_selected)))

md_selected <- md_selected %>%
  select(all_of(names(np_selected)))

sl_selected <- sl_selected %>%
  select(all_of(names(np_selected)))

# Add country labels to the datasets
af_selected$country <- "Afghanistan"
bd_selected$country <- "Bangladesh"
bt_selected$country <- "Bhutan"
md_selected$country <- "Maldives"
np_selected$country <- "Nepal"
sl_selected$country <- "Sri Lanka"

# Bind the rows of all selected datasets
df <- bind_rows(af_selected, bd_selected, md_selected, np_selected, sl_selected)

# Copy the labels from the Nepal dataset to the combined dataframe
df <- labelled::copy_labels(np_selected, df)

#csv file of final data

saveRDS(df, here('data', 'clean_dataset', 'clean_dataset_for_analysis.rds'))

#clearing the previous environment and loading packages
rm(list=ls())

packages <- c("gtsummary","foreign","survey",'labelled',"readxl", "tidyverse",
              "haven","rockchalk", "forcats", "data.table", "Hmisc", "srvyr", "here")

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
invisible(lapply(packages, require, character.only=T))

#read clean dataset for analysis
df <- readRDS(here('data', 'clean_dataset', 'clean_dataset_for_analysis.rds'))


#Cleaning Variables for Analysis
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

# q26 attempted suicide

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
               q40 %in% c("7 years old or younger", "8 or 9 years old", "10 or 11 years old", "12 or 13 years old") ~ " <13 years)",
               q40 %in% c("14 or 15 years old", "16 or 17 years old", "18 years old or older") ~  "13-18 years)",
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

cat_vars <- c("country","q2", "age_category", "education_cat","substance_use",                                "drug_initiation_cat", "hunger_cat", "miss_school_cat",
              "felt_lonely",  "q24", 
              "q17", "q18",
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

#===================================== logistic model for our analysis
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
alluvial_plot <- df2 %>%
  mutate(
    fight = factor(physical_fight_binary, labels = c("Yes", "No")),
    substance = factor(substance_use, labels = c("No", "Yes", "Two", "Three")),
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
  ggplot(aes(y = count,
             axis1 = substance,
             axis2 = parent_understand,
             axis3 = bullying, 
             axis4 = hunger,
             axis5 = fight)) + 
  geom_alluvium(aes(fill = substance),  
                alpha = 0.8, 
                width = 1/3,
                knot.pos = 0.4) + 
  geom_stratum(width = 1/4, 
               fill = "white", 
               color = "darkgrey",
               alpha = 0.8) +
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
  annotate("text", x = 1:5, y = rep(0.00001, 5),
           label = c("Substance\nUse", "Parental\nUnderstanding", 
                     "Bullying\nExperience", "Hunger\nFrequency",
                     "Physical\nFight"),  
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


ggsave(filename = here("results", "figures", "alluvial_plot.jpeg"), plot = alluvial_plot)

print(alluvial_plot)
