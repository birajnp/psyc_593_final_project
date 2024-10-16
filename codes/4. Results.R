
source(here("codes/03_data_clean_and_harmonization.R"))


# converting the data type as numberic

df <- df %>% 
  mutate (
    weight = as.numeric(weight),
    psu = as.numeric(psu),
    stratum = as.numeric(stratum)
  )

# Create the survey design object 
svy_dataset <- svydesign(
  data = df,                      # Use the entire dataset
  ids = ~ psu,                       # Primary Sampling Units variable
  strata = ~ stratum,                # Stratum variable
  weights = ~ weight, nest = TRUE                # Weights variable
)


# Summary statistics
summary_stats <- svy_dataset %>%
  tbl_svysummary(
    include = c('q1', 'q2', 'q3',                         # Age, Sex, Grade
                'q16',                        # Physical fighting 
                'q20', 'q22', 'q23',                               # bullied, Mental health: Felt lonely, could not sleep
                'q24', 'q25', 'q26', 'q27',   #
                'q28', 'q29', 'q30', 'q33',   # Tobacco use: Initiation, current use, cessation, exposure, parental use
                'q34', 'q35',  # Alcohol use: Initiation, current use, frequency, source, effects
                'q40','q41', 'q42', 
                'q56', 'q57', 'q58',        # parent understand problem, know about free time, go through things
                'q53', 'q54', # CLASS ATTENDANCE AND PEER SUPPORT
                
                # outcome Variables
                'q44', 'q45', 'q47', 'q48'),
    statistic = list(all_categorical() ~ "{n} ({p})", all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ c(0,1), all_continuous() ~ c(1,1,1))
  ) %>%
  add_ci(
    method = all_continuous() ~ "svymean",
    style_fun = list(all_categorical() ~ purrr::partial(style_percent, digits = 1),
                     all_continuous() ~ purrr::partial(style_number, digits = 1))
  )

print(summary_stats)

table(data_bhutan$q46)
table(data_nepal$q46)
table(df$q45)

table(data_bangladesh$q45)

table(df$q45)


table(df$q44, df$q1)
