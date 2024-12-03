fit_glm_by_country <- function(svy_dataset) {
  countries <- unique(svy_dataset$variables$country)
  model_results <- list()
  
  for (country in countries) {
    tryCatch({
      if(country == "Bhutan") next
      
      cat("\nProcessing", country, "\n")
      
      # Create a domain indicator for the current country
      domain_indicator <- svy_dataset$variables$country == country
      
      # Subset using domain analysis
      country_svy <- subset(svy_dataset, domain_indicator)
      
      n_obs <- sum(domain_indicator)
      cat("Sample size:", n_obs, "rows\n")
      
      if(n_obs < 30) {
        cat("Skipping: insufficient data\n")
        next
      }
      
      # Fit model for this country
      country_model <- svyglm(physical_fight_binary ~ 
                                q2 + q1 + education + felt_lonely + 
                                couldnot_sleep + q17 + q18 + q20 + 
                                current_marijuna_use + q40 + 
                                smoking_status + q54 + q6 + 
                                miss_school_cat,
                              family = quasibinomial(),
                              design = country_svy)
      
      model_results[[country]] <- country_model
      
    }, error = function(e) {
      cat("Error for", country, ":", e$message, "\n")
    })
  }
  return(model_results)
}

# Run the models
country_models <- fit_glm_by_country(svy_dataset)

names(country_models)  # Should show us which countries have fitted models

# For each country with a model, let's look at key coefficients:
for(country in names(country_models)) {
  cat("\n\nResults for:", country, "\n")
  print(summary(country_models[[country]]))
}


# For each country:
for(country in names(country_models)) {
  # Check for complete separation
  print(paste("Checking", country))
  print(table(country_models[[country]]$y))
  print(summary(country_models[[country]]$fitted.values))
}










# Modified function with simplified model and better handling
fit_glm_by_country_improved <- function(svy_dataset) {
  countries <- unique(svy_dataset$variables$country)
  model_results <- list()
  
  # Key predictors based on Maldives results
  simplified_formula <- physical_fight_binary ~ 
    q2 +                    # Gender (significant)
    age_category +          # Simplified age categories
    q17 +                   # Injury frequency (significant)
    q20 +                   # Bullying (significant)
    smoking_status +        # Smoking (significant)
    q54 +                   # Student kindness (significant)
    current_marijuna_use    # Marijuana use (significant)
  
  for (country in countries) {
    tryCatch({
      if(country == "Bhutan") next
      
      cat("\nProcessing", country, "\n")
      
      # Create domain indicator
      domain_indicator <- svy_dataset$variables$country == country
      country_svy <- subset(svy_dataset, domain_indicator)
      
      n_obs <- sum(domain_indicator)
      cat("Sample size:", n_obs, "\n")
      
      # Fit simplified model
      model <- svyglm(simplified_formula,
                      family = quasibinomial(),
                      design = country_svy)
      
      model_results[[country]] <- model
      
    }, error = function(e) {
      cat("Error for", country, ":", e$message, "\n")
    })
  }
  return(model_results)
}

# Run improved models
improved_models <- fit_glm_by_country_improved(svy_dataset)

# Function to compare key predictors across countries
compare_country_effects <- function(models) {
  results <- data.frame()
  
  for(country in names(models)) {
    model_summary <- summary(models[[country]])
    coefs <- model_summary$coefficients
    
    # Extract key predictors
    key_vars <- c("q2Female", "current_marijuna_use1", "smoking_status1", "q541")
    
    for(var in key_vars) {
      if(var %in% rownames(coefs)) {
        row <- data.frame(
          Country = country,
          Predictor = var,
          Estimate = coefs[var, "Estimate"],
          SE = coefs[var, "Std. Error"],
          OR = exp(coefs[var, "Estimate"]),
          P_value = coefs[var, 4]
        )
        results <- rbind(results, row)
      }
    }
  }
  return(results)
}

# Compare results across countries
comparison <- compare_country_effects(improved_models)
print(comparison)

# Check model diagnostics
for(country in names(improved_models)) {
  cat("\nModel fit diagnostics for", country, "\n")
  print(summary(improved_models[[country]]))
}









