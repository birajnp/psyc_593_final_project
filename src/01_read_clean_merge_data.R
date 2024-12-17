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

#data_bhutan <- read_dta(here("data", "raw_datasets", "3_Bhutan2016.dta"))
#data_bhutan <- forcats::as_factor(data_bhutan, only_labelled = TRUE)

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

#bhutan_labels <- get_var_label (data_bhutan, "Bhutan")

maldives_labels <- get_var_label (data_maldives, "Maldives")

nepal_labels <- get_var_label (data_nepal, "Nepal")

srilanka_labels <- get_var_label (data_srilanka, "Srilanka")

# Combine all labels into one data frame
combined_label <- bind_rows(afghanisthan_labels, bangladesh_labels, 
                           maldives_labels, nepal_labels, 
                            srilanka_labels) #bhutan_labels,

# Write the combined labels to a CSV file for comparison
#write_csv(combined_label, 'combined_data_labels_raw.csv')


#Procedure to combine data set, from our analysis we saw that the data file doesn't have consistent name so we need to lowercase the variable name across all data set.
# lower casing the variables name

names(data_afghanisthan) <- tolower(names(data_afghanisthan))

names(data_bangladesh) <- tolower(names(data_bangladesh))

#names(data_bhutan) <- tolower(names(data_bhutan))

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

#bt_selected <- data_bhutan %>% select(all_of(core_vars))

md_selected <- data_maldives %>% select(all_of(core_vars))

np_selected <- data_nepal %>% select(all_of(core_vars))

sl_selected <- data_srilanka %>% select(all_of(core_vars))

# extracting column name for each dataset
af_cols <- names(af_selected) #Afghanisthan

bd_cols <- names(bd_selected) #Bangladesh

#bt_cols <- names(bt_selected) #Bhutan

md_cols <- names(md_selected) #Maldives

np_cols <- names(np_selected) #Nepal

sl_cols <- names(sl_selected) #Srilanka

# Checking if column names match across all datasets
cols_match <- cols_match <- all(
  bd_cols == md_cols & 
 #   bt_cols == np_cols & 
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
#bt_selected <- convert_all_to_factors(bt_selected)  # Bhutan
md_selected <- convert_all_to_factors(md_selected)  # Maldives
np_selected <- convert_all_to_factors(np_selected)  # Nepal
sl_selected <- convert_all_to_factors(sl_selected)  # Sri Lanka


# Ensure all datasets have the same columns as Nepal
af_selected <- af_selected %>%
  select(all_of(names(np_selected)))


bd_selected <- bd_selected %>%
  select(all_of(names(np_selected)))

#bt_selected <- bt_selected %>%
 # select(all_of(names(np_selected)))

md_selected <- md_selected %>%
  select(all_of(names(np_selected)))

sl_selected <- sl_selected %>%
  select(all_of(names(np_selected)))

# Add country labels to the datasets
af_selected$country <- "Afghanistan"
bd_selected$country <- "Bangladesh"
#bt_selected$country <- "Bhutan"
md_selected$country <- "Maldives"
np_selected$country <- "Nepal"
sl_selected$country <- "Sri Lanka"

# Bind the rows of all selected datasets
df <- bind_rows(af_selected, bd_selected, md_selected, np_selected, sl_selected)

# Copy the labels from the Nepal dataset to the combined dataframe
df <- labelled::copy_labels(np_selected, df)

#csv file of final data

saveRDS(df, here('data', 'clean_dataset', 'clean_dataset_for_analysis.rds'))