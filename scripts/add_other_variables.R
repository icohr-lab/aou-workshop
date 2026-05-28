library(tidyverse)

cleaned_dataset <- read_csv("data/cleaned_dataset.csv")

# Set a seed to ensure all workshop participants generate the exact same synthetic values
set.seed(42)

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    true_exposure_status = case_when(
      # Condition 1: No record of taking the drug
      is.na(drug_exposure_start_date) ~ "Unexposed",
      # Condition 2: Took the drug, but never got the condition
      is.na(condition_start_date) ~ "Exposed",
      # Condition 3: Took the drug BEFORE the condition started
      drug_exposure_start_date < condition_start_date ~ "Exposed",
      # Condition 4: Took the drug AFTER the condition started
      drug_exposure_start_date >= condition_start_date ~ "Unexposed (Post-Diagnosis)"
    )
  )

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    # Generate SBP values
    SBP = round(90 + (0.5 * (2026 - year_of_birth)) + rnorm(n(), mean = 0, sd = 10))
  )

# Maintain the seed for workshop reproducibility
set.seed(42)

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    # Randomly select 5 rows to replace with NA
    SBP = if_else(
      condition = row_number() %in% sample(n(), size = 5), 
      true = NA_real_, 
      false = SBP
    )
  )

# Save to a file
write_csv(cleaned_dataset, "data/final_dataset.csv")


# Additional variables for easy start in Session 4

cleaned_dataset <- cleaned_dataset %>% mutate(
  age = 2026 - year_of_birth
)

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    condition_status = if_else(is.na(condition_concept_id), 0, 1), # 0 if condition_concept_id is NA, 1 if not
    exposure_status = if_else(true_exposure_status == "Exposed", 1, 0) # 1 if true_exposure_status is "Exposed", 0 if not
  )

# Save to a separate file
write_csv(cleaned_dataset, "data/final_dataset2.csv")

