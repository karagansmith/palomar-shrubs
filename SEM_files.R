
#bring in data 
library(readr)
sem_data <- read_csv("Desktop/Palamar/Shrub Recovery/sem_data.csv")

#make a summary stats table 

library(dplyr)
library(tidyr)

sem_data <- sem_data %>%
  mutate(BurnTreat = case_when(
    plot %in% 1:12  ~ "Moderate_Burn",
    plot %in% 13:24 ~ "Severe_Burn",
    plot %in% 25:36 ~ "Unburned"
  ))

summary_by_burn <- sem_data %>%
  group_by(BurnTreat) %>%
  summarise(
    across(
      .cols = c(twig_dm, invasive_cover, total_cover, relative_invasive, n_seedlings_total, `2022_survival`),
      .fns = list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -BurnTreat,
    names_to = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"   # split only at last underscore
  )

summary_by_burn


library(lavaan)
library(dplyr)

# Filter to plots 1–24
sem_subset <- sem_data %>% filter(plot <= 24)

sem_subset <- sem_data %>%
  filter(plot <= 24) %>%
  rename(survival2022 = `2022_survival`)


# Step 1: Create fire severity (higher = more severe)
sem_subset$fire_severity <- scale(sem_subset$twig_dm)

#for publication table

library(dplyr)
library(tidyr)

sem_subset2 <- sem_subset %>%
  mutate(
    BurnTreat = case_when(
      plot %in% 1:12  ~ "Moderate_Burn",
      plot %in% 13:24 ~ "Severe_Burn"
    )
  )

summary_by_burn <- sem_subset2 %>%
  group_by(BurnTreat) %>%
  summarise(
    across(
      .cols = c(fire_severity, twig_dm, invasive_cover, total_cover,
                relative_invasive, n_seedlings_total, survival2022),
      .fns = list(mean = ~mean(.x, na.rm = TRUE),
                  sd   = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -BurnTreat,
    names_to = c("variable", ".value"),
    names_sep = "_(?=[^_]+$)"   # split only at last underscore
  ) %>%
  mutate(mean_sd = sprintf("%.2f ± %.2f", mean, sd)) %>%
  select(BurnTreat, variable, mean_sd)

summary_by_burn


#we need to log transform because it is zero infated (publication)
sem_subset$log_invasive_cover <- log(sem_subset$invasive_cover + 1)

# Step 2: Scale numeric variables for stability
numeric_vars <- c("fire_severity", "log_invasive_cover", "n_seedlings_total", "survival2022")
sem_subset_scaled <- sem_subset
sem_subset_scaled[numeric_vars] <- scale(sem_subset_scaled[numeric_vars])

# Step 3: Define the SEM model with direct and indirect effects
model_indirect <- '
  # Regressions
  log_invasive_cover ~ a1*fire_severity
  n_seedlings_total ~ b1*fire_severity + b2*log_invasive_cover
  survival2022 ~ c1*fire_severity + c2*log_invasive_cover + c3*n_seedlings_total

  # Indirect effects
  indirect_fire_to_seedlings := a1 * b2
  indirect_fire_to_survival_via_invasive := a1 * c2
  indirect_fire_to_survival_via_seedlings := b1 * c3
  indirect_fire_to_survival_via_both := a1 * b2 * c3

  # Total effects
  total_fire_to_seedlings := b1 + (a1 * b2)
  total_fire_to_survival := c1 + (a1 * c2) + (b1 * c3) + (a1 * b2 * c3)
'

# Step 4: Fit the SEM on scaled data
fit_scaled <- sem(model_indirect, data = sem_subset_scaled, meanstructure = TRUE)

# Step 5: View summary with standardized coefficients and fit measures
summary(fit_scaled, standardized = TRUE, fit.measures = TRUE)

# Optional: Inspect variable variances (if needed)
varTable(fit_scaled)
# library(lavaan)

fit_scaled <- sem(model_indirect, data = sem_subset, meanstructure = TRUE)


#DROP SEEDLING ABUNDANCE to SEEDLING SURVIVAL to make the SEM non saturated 

library(lavaan)

# Define the SEM model (non-saturated, drops seedling abundance → survival)
model_non_saturated <- '
  # Direct effects
  log_invasive_cover ~ a*fire_severity
  n_seedlings_total ~ b*log_invasive_cover + c*fire_severity
  survival2022 ~ d*log_invasive_cover + e*fire_severity

  # Indirect effects
  indirect_seedlings := a*b
  indirect_survival := a*d

  # Total effects
  total_seedlings := c + indirect_seedlings
  total_survival := e + indirect_survival
'

# Fit the model
fit_non_saturated <- sem(
  model_non_saturated,
  data = sem_subset,
  estimator = "ML",
  meanstructure = TRUE,
  fixed.x = FALSE
)

# Summary: standardized solution and fit measures
summary(fit_non_saturated, standardized = TRUE, fit.measures = TRUE)

#figure made in powerpoint 