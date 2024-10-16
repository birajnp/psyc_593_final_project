rm(list=ls())

# Project : Do Adolescents Emotions Turn into Physical Outbursts?
# load packages

Packages <- c("gtsummary","foreign","survey",'labelled',"readxl", "tidyverse",
              "haven","rockchalk", "forcats", "data.table", "Hmisc", "srvyr", "here")

new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

#load libraries
lapply(Packages, require, character.only=T)


# read data set
# Data Set Afghanistan 2014

data_afghanisthan <- read_dta(here("raw_datasets", "1_Afghanistan2014.dta"))
data_afghanisthan <- forcats::as_factor(data_afghanisthan, only_labelled = TRUE)

fwrite(data.table(Column_Name = names(data_afghanisthan), 
                  Label = sapply(var_label(data_afghanisthan), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_afghanisthan.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)


# Data Set Bangladesh 2014
data_bangladesh <- read_dta(here("raw_datasets","2_Bangladesh2014.dta"))
data_bangladesh <- forcats::as_factor(data_bangladesh, only_labelled = TRUE)

# Write the data.table to a file while replacing NULL values 
fwrite(data.table(Column_Name = names(data_bangladesh), 
                  Label = sapply(var_label(data_bangladesh), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_bangladesh.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)


#Data set Bhutan 2016

data_bhutan <- read_dta(here("raw_datasets", "3_Bhutan2016.dta"))
data_bhutan <- forcats::as_factor(data_bhutan, only_labelled = TRUE)

# Write the data.table to a file while replacing NULL values 
fwrite(data.table(Column_Name = names(data_bhutan), 
                  Label = sapply(var_label(data_bhutan), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_bhutan.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)


# Data Set Maldives 2014

data_maldives <- read_dta(here("raw_datasets", "4_Maldives2014.dta"))
data_maldives <- forcats::as_factor(data_maldives, only_labelled = TRUE)

# Write the data.table to a file while replacing NULL values 
fwrite(data.table(Column_Name = names(data_maldives), 
                  Label = sapply(var_label(data_maldives), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_maldives.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)




# Data Set Nepal 2015

data_nepal <- read_dta(here("raw_datasets", "5_Nepal2015.dta"))
data_nepal <- forcats::as_factor(data_nepal, only_labelled = TRUE)

fwrite(data.table(Column_Name = names(data_nepal), 
                  Label = sapply(var_label(data_nepal), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_nepal.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)

# Data Set Srilanka 2016

data_srilanka <- read_dta(here("raw_datasets", "6_Srilanka2016.dta"))
data_srilanka <- forcats::as_factor(data_srilanka, only_labelled = TRUE)

fwrite(data.table(Column_Name = names(data_srilanka), 
                  Label = sapply(var_label(data_srilanka), 
                                 function(x) ifelse(is.null(x), "", x))),
       "column_labels_srilanka.txt", sep = ";", 
       col.names = TRUE, 
       row.names = FALSE)

###-----------------------------------------------------------------------------


# function to get variable labels (if present) or just names
get_var_label <- function(data, dataset_name) {
  data.frame(
    Column_Name = colnames(data),
    Label = sapply(var_label(data), 
                   function(x) ifelse(is.null(x), "", x)),
    Dataset = dataset_name
  )
}

# Get variable labels from each dataset
afghanisthan_labels <- get_var_label(data_afghanisthan, "Afghanisthan")
bangladesh_labels <- get_var_label(data_bangladesh, "Bangladesh")
bhutan_labels <- get_var_label(data_bhutan, "Bhutan")
maldives_labels <- get_var_label(data_maldives, "Maldives")
nepal_labels <- get_var_label(data_nepal, "Nepal")
srilanka_labels <- get_var_label(data_srilanka, "Srilanka")

# Combine all labels into one data frame
combined_label <- bind_rows(afghanisthan_labels, bangladesh_labels, bhutan_labels, maldives_labels, nepal_labels, srilanka_labels )

# Write the combined labels to a CSV file for comparison
write_csv(combined_label, 'combined_data_labels_raw.csv')

