
library(here)
source(here("codes", "01_libraries_and_dataset.R"))

# lowercasing the variables name

names(data_afghanisthan) <- tolower(names(data_afghanisthan))
names(data_bangladesh) <- tolower(names(data_bangladesh))
names(data_bhutan) <- tolower(names(data_bhutan))
names(data_maldives) <- tolower(names(data_maldives))
names(data_nepal) <- tolower(names(data_nepal))
names(data_srilanka) <- tolower(names(data_srilanka))



# List of required variables 

# core_vars <- c(
#   # independent variables
#   'Q1', 'Q2', 'Q3',                         # Age, Sex, Grade
#   'Q28', 'Q29', 'QN29', 'Q30', 'Q31', 'Q32', 'Q33',   # Tobacco use: Initiation, current use, cessation, exposure, parental use
#   'Q42', 'QN42',
#   'Q34', 'Q35', 'QN35', 'Q36', 'Q37', 'Q38', 'Q39',   # Alcohol use: Initiation, current use, frequency, source, effects
#   'Q22', 'Q23',                              # Mental health: Felt lonely, could not sleep
#   'Q24', 'Q25', 'QN25', 'Q26', 'QN26', 
#   'Q16', 'Q17', 'Q18',                        # Physical fighting, seriously injured, serious injury tyope
#   'Q20', 'Q22',                                # Bullied, felt lonely
#   'Q56', 'Q57', 'Q58',        # parent understand problem, know about free time, go throught things
#   # outcome Variables
#   'Q44', 'QN44', 'Q45', 'Q47', 'QN47', 'Q48','qnc1g',          # Sexual behaviors: Intercourse, age of first sex, condom, birth control
#   
#   #sample weight and related variable
#   'weight', 'stratum', 'psu'                 # Sample weights and design variables
# )

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

# # making format of the variable same in Nepal as other have upper case
# names(bd_selected)<-toupper(names(bd_selected))
# names(bt_selected) <- toupper(names(bt_selected))
# names(np_selected) <- toupper(names(np_selected))

# mutate to add country column in each selected varible dataset

#bd_selected <- bd_selected %>% mutate(Country = "Bangladesh")
#bt_selected <- bt_selected %>% mutate(Country = "Bhutan")
#np_selected <- np_selected %>% mutate(Country = "Nepal")

# Function to rename columns in Nepal dataset to match other datasets #name not consistant
#rename_columns_nepal <- function(df) {
  #df %>%
#    rename(
#      Q1 = q1,
#      Q2 = q2,
#      Q3 = q3,
#      Q28 = q28,
#      Q29 = q29,
#      Q30 = q30,
#      Q31 = q31,
#      Q32 = q32,
#     Q33 = q33,
#     Q42 = q42,
#      Q34 = q34,
#      Q35 = q35,
 #     Q36 = q36,
#      Q37 = q37,
  #    Q38 = q38,
 #     Q39 = q39,
#      Q22 = q22,
#      Q23 = q23,
#      Q24 = q24,
#      Q25 = q25,
#      Q26 = q26,
#      Q16 = q16,
#      Q17 = q17,
#      Q18 = q18,
#      Q20 = q20,
#      Q56 = q56,
#      Q57 = q57,
#      Q58 = q58,
#      Q44 = q44,
#      Q45 = q45,
#      Q47 = q47,
#     Q48 = q48,
#     qnc1g = qnc1g,
#      QN44 = qn44,
#      QN47 = qn47,
#      QN25 = qn25,
#      QN26 = qn26,
#     QN42 = qn42,
#      QN35 = qn35,
#      Q36 = q36,
#      QN29 = qn29,
#      weight = weight,
#      stratum = stratum,
#      psu = psu
#   )}


#np_selected <- rename_columns_nepal(np_selected)`

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


# Saving the selected datasets to CSV files in the 'clean_dataset' folder
write_csv(af_selected, here('clean_dataset', 'selected_afghanistan_data.csv'))
write_csv(bd_selected, here('clean_dataset', 'selected_bangladesh_data.csv'))
write_csv(bt_selected, here('clean_dataset', 'selected_bhutan_data.csv'))
write_csv(md_selected, here('clean_dataset', 'selected_maldives_data.csv'))
write_csv(np_selected, here('clean_dataset', 'selected_nepal_data.csv'))
write_csv(sl_selected, here('clean_dataset', 'selected_sri_lanka_data.csv'))


# required as some data type deffered function to convert all columns to factors
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


# Define a named vector with variable names as names and labels as values
#var_labels <- c(
#  Q1 = "Custom Age",
#  Q2 = "Sex",Q3 = "In what grade are you",
#  Q28 = "Initiation of cigarette use",
#  Q29 = "Current cigarette use",
#  QN29 = "Current cigarette use",
#  Q30 = "Other tobacco use",
#  Q31 = "Smoking cessation",
#  Q32 = "Smoking in their presence",
#  Q33 = "Parental tobacco use",
#  Q42 = "Current marijuana use",
#  QN42 = "Current marijuana use",
#  Q34 = "Initiation of alcohol use",
#  Q35 = "Current alcohol use",
#  QN35 = "Current alcohol use",
#  Q36 = "Drank 2+ drinks",
#  Q37 = "Source of alcohol",
#  Q38 = "Really drunk",
#  Q39 = "Trouble from drinking",
#  Q22 = "Felt lonely",
#  Q23 = "Could not sleep",
#  Q24 = "Considered suicide",
#  Q25 = "Made a suicide plan",
#  QN25 = "Made a suicide plan",
#  Q26 = "Attempted suicide",
#  QN26 = "Attempted suicide",
#  Q16 = "Physical fighting",
#  Q17 = "Seriously injured",
#  Q18 = "Serious injury broken bone",
#  Q20 = "Bullied",
#  Q56 = "Parents understand problems",
#  Q57 = "Parents know about free time",
#  Q58 = "Parents go through their things",
#  Q44 = "Ever sexual intercourse",
#  QN44 = "Ever sexual intercourse",
#  Q45 = "Sex before 14 years",
#  Q47 = "Condom use",
#  QN47 = "Condom use",
#  Q48 = "Birth control used",
#  qnc1g = "Ever had sexual intercourse among students who had drank so much alcohol that they were really drunk",
#  weight = "Sample weight",
#  stratum = "Stratum",
#  psu = "PSU")


  # add labels to df #for (var in names(var_labels)) { # if (var %in% names(df)}
 #   label(df[[var]]) <- var_labels[var]}}


# Bind the rows of all selected datasets
df <- bind_rows(af_selected, bd_selected, bt_selected, md_selected, np_selected, sl_selected)

# Copy the labels from the Nepal dataset to the combined dataframe
df <- labelled::copy_labels(np_selected, df)

# structure of the new dataframe
str(df)

#csv file of final data

write_csv(df, here('clean_dataset', 'clean_dataset_for_analysis.csv'))

#done 
