#large scale plot seedling numbers and survival of 2022 cohort 

#bring in the data 
library(readr)
shrubs<- read_csv("~/Desktop/Palamar/Shrub Recovery/allshrubspalomar.csv")

#bring in treatments data that includes include exclosures
treatments <- read_csv("~/Desktop/Palamar/Treatments Files/treatments_invasion.csv")


library(dplyr)

# Rename BurnTreat values in the treatments dataset
treatments_unique <- treatments %>%
  select(-Exclosure) %>%  # Exclude the Exclosure column
  mutate(BurnTreat = case_when(
    BurnTreat == "BurnedInvaded" ~ "Moderate_Burn",
    BurnTreat == "BurnedNative" ~ "Severe_Burn",
    TRUE ~ BurnTreat  # Keep other categories unchanged
  ))
#check duplicates 
treatments_unique %>%
  count(Plot) %>%
  filter(n > 1)
#get rid of duplicates 
treatments_unique <- treatments_unique %>%
  distinct(Plot, BurnTreat)
# Join the cleaned treatments data to seedlings_data
shrubs <- shrubs %>%
  left_join(treatments_unique, by = "Plot", relationship = "many-to-many")

#switch DW to AG and YS to EC ##this is to match the correct latin scientifc name abbrevation instead fo the common name we used in the filed notebook 
# Replace species names
shrubs <- shrubs %>%
  mutate(Species = case_when(
    Species == "DW" ~ "AG",
    Species == "YS" ~ "EC",
    TRUE ~ Species  # Keep other species unchanged
  ))

library(tidyr)  # For replace_na()

# Replace NA in Exclosure with "no"
shrubs <- shrubs %>%
  mutate(Exclosure = replace_na(Exclosure, "no"))

# Filter seedlings from 2023 that are NOT "new" or "new_large"
shrubs_2022 <- shrubs %>%
  filter(Year == 2023 & !Notes %in% c("new", "new_large")) %>%
  mutate(
    Year = 2022,  # Change Year to 2022
    Status = ifelse(Status == "dead", "alive", Status)  # If dead in 2023, alive in 2022
  )

# Append the new 2022 rows to the original dataset
shrubs <- bind_rows(shrubs, shrubs_2022) %>%
  arrange(Plot, Tag, Year)  # Arrange for better readability

# Identify dead seedlings in 2024
dead_2024 <- shrubs %>%
  filter(Year == 2024 & Status == "dead")

# Check if they have a 2023 entry
missing_2023 <- dead_2024 %>%
  anti_join(shrubs %>% filter(Year == 2023), by = c("Plot", "Tag", "Species", "Type")) %>%
  mutate(Year = 2023, Status = "alive")  # Create new 2023 row with Status = "alive"

# Ensure existing 2023 records are "alive" if they exist
shrubs <- shrubs %>%
  mutate(Status = ifelse(Year == 2023 & Tag %in% dead_2024$Tag, "alive", Status))

# Append new 2023 rows to the dataset
shrubs <- bind_rows(shrubs, missing_2023) %>%
  arrange(Plot, Tag, Year)  # Arrange for readability

# Identify all seedlings that appeared in each year
cohorts <- shrubs %>%
  arrange(Plot, Tag, Year) %>%
  group_by(Plot, Tag) %>%
  mutate(
    FirstYear = min(Year, na.rm = TRUE),  # Find the first year the seedling appeared
    Cohort = case_when(
      FirstYear == 2022 ~ "Cohort_2022",
      FirstYear == 2023 ~ "Cohort_2023",
      FirstYear == 2024 ~ "Cohort_2024",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# Assign cohorts to "new" seedlings explicitly from Notes and Tag
cohorts <- cohorts %>%
  mutate(
    Cohort = case_when(
      Notes %in% c("new", "new_large") | Tag == "new" ~ paste0("Cohort_", Year),
      TRUE ~ Cohort
    )
  )

# Keep only the first recorded year of each seedling and remove duplicates
cohorts <- cohorts %>%
  group_by(Plot, Tag, Species, Type) %>% 
  filter(Year == min(Year)) %>%  # Keep only the first year
  distinct() %>%  # Remove any exact duplicates
  ungroup()

#data by plot 
seedlings_per_plot <- cohorts %>%
  filter(Type == "seedling") %>%
  group_by(Plot) %>%
  summarise(n_seedlings = n()) %>%
  ungroup()

# Count the number of seedlings per cohort
cohort_counts <- cohorts %>%
  count(Cohort)

library(ggplot2)

##PLOT FOR PUB#######
# 2. Create unburned rows for all cohort levels
unburned_data <- data.frame(
  BurnTreat = "Unburned",
  Cohort = factor(c("Cohort_2022", "Cohort_2023", "Cohort_2024")),
  Mean_Count = 0,
  SE = 0
)

# 3. Combine with your main data
cohort_full <- rbind(cohort_summary, unburned_data)

# 4. Plot
# Rename BurnTreat levels
# Replace underscores with spaces
cohort_full$BurnTreat <- gsub("_", " ", cohort_full$BurnTreat)


ggplot(cohort_full, aes(x = Cohort, y = Mean_Count, fill = BurnTreat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Count - SE, ymax = Mean_Count + SE),
                position = position_dodge(width = 0.7), width = 0.4, size =2) +
  scale_fill_manual(values = c("Unburned" = "darkseagreen",
                               "Moderate Burn" = "#D88A25", 
                               "Severe Burn" = "cadetblue")) +
  scale_x_discrete(labels = c("2022", "2023", "2024")) + 
  labs(title = "",
       x = "Cohort Year",
       y = "Shrub seedlings (# per 9 m²)",
       fill = "") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 55),
    axis.text.y = element_text(size = 55),
    axis.title = element_text(size = 60,face = "bold"),
    legend.text = element_text(size = 50),
    legend.title = element_text(size = 40),
    legend.position = c(0.75, 0.85),  # move legend inside
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

#now lets do stats! 

# Required packages
library(dplyr)
library(tidyr)
library(emmeans)
library(car)        # for Anova()
library(broom)      # for tidy() (optional, for output tables)

# apply the correct treatments to plots 1-24 
plot_info <- data.frame(
  Plot = 1:24,
  BurnTreat = c(rep("Moderate_Burn", 12), rep("Severe_Burn", 12)),
  Drought = ifelse(1:24 %% 2 == 1, "Drought", "Ambient")
)

#define the full grid to add in zeros when no seedlings were found 
years <- c(2022, 2023, 2024)
full_grid <- expand.grid(
  Plot = 1:24,
  Year = years,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  left_join(plot_info, by = "Plot")

# Merge full grid with counts, fill missing with zeros
# Count seedlings per plot and year
cohort_counts <- cohorts %>%
  filter(Type == "seedling") %>%
  group_by(Plot, Year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # Make sure columns match full_grid
  mutate(
    Plot = as.integer(Plot),
    Year = as.integer(Year)
  )

#make full grid
years <- 2022:2024

full_grid <- expand.grid(
  Plot = 1:24,
  Year = years
) %>%
  # ensure integer types
  mutate(
    Plot = as.integer(Plot),
    Year = as.integer(Year)
  ) %>%
  # add treatments
  left_join(plot_info, by = "Plot")

#merge cohort counts into full_grid
cohort_complete <- full_grid %>%
  left_join(cohort_counts, by = c("Plot", "Year")) %>%
  mutate(
    Count = ifelse(is.na(Count), 0, Count),
    Year = factor(Year),
    BurnTreat = factor(BurnTreat),
    Drought = factor(Drought)
  )

#Fit GLM 

cohort_complete$Year <- as.factor(cohort_complete$Year)

glm_quasi <- glm(
  Count ~ Year * Drought * BurnTreat,
  family = quasipoisson(link = "log"),
  data = cohort_complete
)

# Type III ANOVA
Anova(glm_quasi, type = "III")

library(emmeans)

# Estimated marginal means for Year
emm_year <- emmeans(glm_quasi, ~ Year)

# Pairwise contrasts with Tukey adjustment
year_contrasts <- contrast(emm_year, method = "pairwise", adjust = "tukey")

# View results
summary(year_contrasts)


###SURVIVAL FOR 2022 COHORT ###

# Get unique 2022 seedlings
shrubs_2022 <- shrubs %>%
  filter(Year == 2022, Type == "seedling") %>%
  select(Plot, Tag, Status_2022 = Status)

# Get 2023 and 2024 statuses for the same seedlings
shrubs_future <- shrubs %>%
  filter(Tag %in% shrubs_2022$Tag & Year %in% c(2023, 2024)) %>%
  group_by(Plot, Tag, Year) %>%
  summarise(Status = ifelse(any(Status == "alive"), "alive", "dead"), .groups = "drop") %>%
  pivot_wider(
    names_from = Year,
    values_from = Status,
    names_prefix = "Status_"
  )

# Combine with 2022 data
shrubs_all_years <- shrubs_2022 %>%
  left_join(shrubs_future, by = c("Plot", "Tag"))

# Step 4: Calculate survival rates
survival_rates <- shrubs_all_years %>%
  summarise(
    n_2022 = n(),
    alive_2023 = sum(Status_2023 == "alive", na.rm = TRUE),
    alive_2024 = sum(Status_2024 == "alive", na.rm = TRUE),
    surv_rate_2023 = alive_2023 / n_2022,
    surv_rate_2024 = alive_2024 / n_2022
  )
shrubs_all_years <- shrubs_all_years %>%
  mutate(
    Status_2023 = replace_na(Status_2023, "dead"),
    Status_2024 = replace_na(Status_2024, "dead")
  )

#now I need to graph and do stats
library(dplyr)
library(tidyr)

# add treatment info (replace with your actual treatment lookup)
treatments <- tibble(
  Plot = 1:24,
  BurnTreat = rep(c("Moderate_Burn", "Severe_Burn"), each = 12),
  Drought = rep(c("Drought", "Ambient"), length.out = 24)
)

# Join treatments
shrubs_with_treat <- shrubs_all_years %>%
  left_join(treatments, by = "Plot")

# Reshape to long format
shrubs_long <- shrubs_with_treat %>%
  pivot_longer(cols = starts_with("Status_"),
               names_to = "Year",
               values_to = "Status") %>%
  mutate(
    Year = gsub("Status_", "", Year),
    Survival = ifelse(Status == "alive", 1, 0)
  )

# Filter 2022 cohort seedlings
shrubs_2022_cohort <- shrubs_long %>%
  filter(Year == "2022") %>%
  select(Plot, Tag, BurnTreat, Drought)

# Keep only survival transitions for 2023 and 2024
shrubs_transitions <- shrubs_long %>%
  filter(Year %in% c("2023", "2024")) %>%
  semi_join(shrubs_2022_cohort, by = c("Plot", "Tag", "BurnTreat", "Drought")) %>%
  arrange(Plot, Tag, Year)

shrubs_transitions_unique <- shrubs_transitions %>%
  group_by(Plot, Tag, BurnTreat, Drought, Year) %>%
  summarise(Survival = max(Survival), .groups = "drop")
# Calculate survival to next year (keep original cohort n as denominator)
survival_summary <- shrubs_transitions_unique %>%
  group_by(BurnTreat, Drought, Year) %>%
  summarise(
    n = n_distinct(Tag),           # original cohort size
    survived = sum(Survival),      
    survival_rate = survived / n[1],
    .groups = "drop"
  )

#make plot and year factors 
shrubs_glmm <- shrubs_transitions_unique %>%
  mutate(
    Plot = as.factor(Plot),
    Year = factor(Year)
  ) %>%
  # create binary response for GLMM
  mutate(Survived_to_next = as.numeric(Survival))

#fit the glm 
library(lme4)

fit_surv <- glmer(
  Survived_to_next ~ BurnTreat * Drought * Year + (1|Plot),
  data = shrubs_glmm,
  family = binomial
)

#check car Anova
summary(fit_surv)
library(car)
Anova(fit_surv, type = "III")

#now plot 

library(dplyr)
library(ggplot2)
library(ggplot2)
library(dplyr)

# Prepare survival summary for plotting
# get the 2022 cohort sizes per BurnTreat
cohort_sizes <- shrubs_transitions_unique %>%
  filter(Year == "2023") %>%  # pick the first survival year
  group_by(BurnTreat) %>%
  summarise(n_cohort = n_distinct(Tag), .groups = "drop")

survival_summary_plot <- shrubs_transitions_unique %>%
  group_by(BurnTreat, Year) %>%
  summarise(
    survived = n_distinct(Tag[Survival == 1]),  # only count unique tags alive
    .groups = "drop"
  ) %>%
  left_join(cohort_sizes, by = "BurnTreat") %>%
  mutate(
    Mean_Count = survived / n_cohort,
    SE = sqrt(Mean_Count * (1 - Mean_Count) / n_cohort),
    NextYear = Year
  ) %>%
  select(BurnTreat, NextYear, n = n_cohort, survived, Mean_Count, SE)


#Add unburned baseline (0 survival)
unburned_data <- data.frame(
  BurnTreat = "Unburned",
  NextYear = factor(c("2023", "2024")),
  n = 1,                # dummy, will not affect SE
  survived = 0,
  Mean_Count = 0,
  SE = 0
)

# Combine with 2022 cohort survival summary
cohort_full <- rbind(survival_summary_plot, unburned_data)

# Clean BurnTreat labels
cohort_full$BurnTreat <- gsub("_", " ", cohort_full$BurnTreat)
cohort_full$NextYear <- factor(cohort_full$NextYear, levels = c("2023", "2024"))

# 4. Plot
ggplot(cohort_full, aes(x = NextYear, y = Mean_Count, fill = BurnTreat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Count - SE, ymax = Mean_Count + SE),
                position = position_dodge(width = 0.7), width = 0.4, size = 1.0) +
  scale_fill_manual(values = c("Unburned" = "darkseagreen",
                               "Moderate Burn" = "#D88A25", 
                               "Severe Burn" = "cadetblue")) +
  labs(x = "Next Year", y = "Survival rate", fill = "") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# 2. Add unburned baseline (optional)
unburned_data <- data.frame(
  BurnTreat = "Unburned",
  NextYear = factor(c("2023", "2024")),
  Mean_Count = 0,
  SE = 0
)

# 3. Combine
cohort_full <- rbind(
  survival_summary_plot[, c("BurnTreat", "NextYear", "Mean_Count", "SE")],
  unburned_data
)

# 4. Plot
ggplot(cohort_full, aes(x = NextYear, y = Mean_Count, fill = BurnTreat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Count - SE, ymax = Mean_Count + SE),
                position = position_dodge(width = 0.7), width = 0.4, size = 1) +
  scale_fill_manual(values = c("Unburned" = "darkseagreen",
                               "Moderate_Burn" = "#D88A25", 
                               "Severe_Burn" = "cadetblue")) +
  labs(title = "",
       x = "Next Year",
       y = "Porportioin of survival (2022 cohort, 9m2)",
       fill = "") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = c(0.75, 0.85),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

#Editied in powerpoint 

#HERBIVORY#
##Survival##

library(dplyr)
library(tidyr)
library(lme4)
library(car)
library(emmeans)

# Track survival by Tag & Plot only
shrubs_future <- shrubs %>%
  filter(Tag %in% shrubs_2022$Tag & Year %in% c(2023, 2024)) %>%
  group_by(Plot, Tag, Year) %>%
  summarise(Status = ifelse(any(Status == "alive"), "alive", "dead"), .groups = "drop") %>%
  pivot_wider(
    names_from = Year,
    values_from = Status,
    names_prefix = "Status_"
  )

# Combine with 2022 cohort
shrubs_all_years <- shrubs_2022 %>%
  left_join(shrubs_future, by = c("Plot", "Tag")) %>%
  # Replace missing Status as dead
  mutate(
    Status_2023 = replace_na(Status_2023, "dead"),
    Status_2024 = replace_na(Status_2024, "dead")
  )


library(tidyverse)

# Convert to long format
shrubs_long <- shrubs_all_years %>%
  pivot_longer(
    cols = starts_with("Status_"),
    names_to = "Year",
    values_to = "Status"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Status_", "", Year))  # convert Status_2022 → 2022
  )
# Calculate survival rates per Plot and Exclosure
survival_summary <- shrubs_long %>%
  group_by(Plot, Exclosure, Year) %>%
  summarize(
    total = n(),
    alive = sum(Status == "alive"),
    survival_rate = alive / total,
    .groups = "drop"
  )

shrubs_2022_cohort <- shrubs_long %>%
  group_by(Tag) %>%
  filter(any(Year == 2022 & Status == "alive")) %>%
  ungroup()

shrubs_2022_cohort <- shrubs_2022_cohort %>%
  mutate(
    Exclosure = factor(Exclosure, levels = c("no", "yes")),
    Plot = factor(Plot),
    Year = factor(Year)
  )

#bring in invasive cover
#I need to bring in the invasive cover as a co variate 

library(readr)
invasive_cover2023 <- read_csv("Desktop/Palamar/Shrub Recovery/invasive_cover2023.csv")

library(readr)
invasive_cover2024 <- read_csv("Desktop/Palamar/Shrub Recovery/invasive_cover2024.csv")


library(dplyr)
glimpse(invasive_cover2023)

# 2023 invasive cover
# 2023 invasive cover
invasive_cover2023 <- invasive_cover2023 %>%
  mutate(
    Exclosure = ifelse(grepl("H$", plot), "yes", "no"),
    Plot = as.numeric(gsub("H", "", plot))
  ) %>%
  select(Plot, Exclosure, invasive_cover) %>%
  mutate(Year = 2023)


# 2024 invasive cover
invasive_cover2024 <- invasive_cover2024 %>%
  mutate(
    Exclosure = ifelse(grepl("H$", plot), "yes", "no"),
    Plot = as.numeric(gsub("H", "", plot))
  ) %>%
  select(Plot, Exclosure, invasive_cover) %>%
  mutate(Year = 2024)

invasive_cover_long <- bind_rows(invasive_cover2023, invasive_cover2024)

# Convert Plot in invasive cover to factor and join 
invasive_cover_long <- invasive_cover_long %>%
  mutate(
    Plot = factor(Plot),
    Exclosure = factor(Exclosure, levels = c("no", "yes")),
    Year = factor(Year)
  )

shrubs_2022_cohort <- shrubs_2022_cohort %>%
  # Remove any rows with missing Exclosure
  filter(!is.na(Exclosure)) %>%
  # Join invasive cover
  left_join(invasive_cover_long, by = c("Plot", "Exclosure", "Year"))

#add treatments 
shrubs_2022_cohort <- shrubs_2022_cohort %>%
  # Add treatments and convert columns for LMM
  mutate(
    Plot_num = as.numeric(as.character(Plot)),  # numeric helper for treatment
    BurnTreat = factor(ifelse(Plot_num <= 12, "Moderate_Burn", "Severe_Burn")),
    Drought = factor(ifelse(Plot_num %% 2 == 1, "Drought", "Ambient")),
    Exclosure = factor(Exclosure, levels = c("no", "yes")),
    Year = factor(Year),
    Status = factor(Status, levels = c("dead", "alive")),
    Survival = ifelse(Status == "alive", 1, 0)  # numeric 0/1 from Status
  ) %>%
  select(-Plot_num)  # drop the helper column

#drop 2022 
shrubs_2022_cohort <- shrubs_2022_cohort %>%
  filter(Year != "2022")

#summaries
shrubs_2022_cohort %>%
  group_by(Year, BurnTreat, Exclosure) %>%
  summarize(
    total = n(),
    alive = sum(Survival),
    survival_rate = alive / total,
    .groups = "drop"
  ) %>%
  arrange(Year, BurnTreat, Exclosure)

library(lme4)
library(car)        # for Anova
library(emmeans)    # for post-hoc contrasts

#log transform invasive cover 
shrubs_2022_cohort <- shrubs_2022_cohort %>%
  mutate(
    invasive_cover_log = log1p(invasive_cover),  # log(1 + x) avoids log(0)
    invasive_cover_scaled = scale(invasive_cover_log)
  )

#make sure you drop 2022 

table(shrubs_2022_cohort$BurnTreat, shrubs_2022_cohort$Exclosure, shrubs_2022_cohort$Year)

shrubs_2022_cohort <- shrubs_2022_cohort %>%
  filter(Year != "2022") %>%       # remove 2022 rows
  droplevels()                     # drop unused factor levels

#set contrasts

options(contrasts = c("contr.sum", "contr.poly"))


# Check counts of Survival by key factors
table(shrubs_2022_cohort$BurnTreat, shrubs_2022_cohort$Exclosure, shrubs_2022_cohort$Survival)

#fit reduced glm 

survival_model_interactions <- glmer(
  Survival ~ BurnTreat * Exclosure + Drought + Year * Exclosure + invasive_cover + (1 | Plot),
  data = shrubs_2022_cohort,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

# Type III ANOVA (marginal effects)
# -----------------------------
anova_table <- Anova(survival_model_interactions, type = 3)
anova_table

#now I want to graph 
library(dplyr)
library(ggplot2)



# Step 1: Summarize survival across plots by BurnTreat, Exclosure, and Year
shrubs_summary_grouped <- shrubs_2022_cohort %>%
  group_by(Year, BurnTreat, Exclosure) %>%
  summarise(
    n_seedlings = n(),
    n_survived = sum(Survival),
    mean_survival = mean(Survival),
    SE = sd(Survival)/sqrt(n())
  ) %>%
  ungroup() %>%
  mutate(dodge_group = paste(BurnTreat, Exclosure, sep = "_"))

# First, create bar_group combining BurnTreat, Exclosure, and Year
shrubs_summary_grouped <- shrubs_summary_grouped %>%
  mutate(bar_group = paste(BurnTreat, Exclosure, Year, sep = "_"))

# Then reorder factor levels for plotting
shrubs_summary_grouped$bar_group <- factor(shrubs_summary_grouped$bar_group,
                                           levels = c(
                                             "Moderate_Burn_no_2023",
                                             "Severe_Burn_no_2023",
                                             "Moderate_Burn_no_2024",
                                             "Severe_Burn_no_2024",
                                             "Moderate_Burn_yes_2023",
                                             "Severe_Burn_yes_2023",
                                             "Moderate_Burn_yes_2024",
                                             "Severe_Burn_yes_2024"
                                           ))


# Step 2: Define colors
burn_colors <- c("Moderate_Burn" = "#D88A25", "Severe_Burn" = "cadetblue")

# Plot
# Define fill based on Exclosure: yes = solid, no = open
shrubs_summary_grouped$fill_color <- ifelse(shrubs_summary_grouped$Exclosure == "yes",
                                            burn_colors[shrubs_summary_grouped$BurnTreat],
                                            "white")

# Define outline color based on BurnTreat
shrubs_summary_grouped$outline_color <- burn_colors[shrubs_summary_grouped$BurnTreat]

# Plot
ggplot(shrubs_summary_grouped, aes(x = bar_group, y = mean_survival)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           aes(fill = fill_color, color = outline_color)) +
  geom_errorbar(aes(ymin = mean_survival - SE, ymax = mean_survival + SE),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_identity() +   # uses the fill_color column directly
  scale_color_identity() +  # uses the outline_color column directly
  labs(x = "", y = "Mean Survival") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with thicker outlines
ggplot(shrubs_summary_grouped, aes(x = bar_group, y = mean_survival)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           aes(fill = fill_color, color = outline_color), size = 1.5) +  # thicker outline
  geom_errorbar(aes(ymin = mean_survival - SE, ymax = mean_survival + SE),
                position = position_dodge(width = 0.8), width = 0.2, size = 1) +  # thicker error bars
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = "", y = "Mean Survival") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#better

library(ggplot2)
library(dplyr)

# Define burn colors
burn_colors <- c("Moderate_Burn" = "#D88A25", "Severe_Burn" = "cadetblue")

# Plot
ggplot(shrubs_summary_grouped, aes(x = bar_group, y = mean_survival)) +
  
  # Open bars for Exclosure == "no"
  geom_bar(data = shrubs_summary_grouped %>% filter(Exclosure == "no"),
           aes(color = BurnTreat),
           stat = "identity",
           fill = NA,
           width = 0.7,
           size = 1.5) +
  
  # Filled bars for Exclosure == "yes"
  geom_bar(data = shrubs_summary_grouped %>% filter(Exclosure == "yes"),
           aes(fill = BurnTreat),
           stat = "identity",
           color = "black",
           width = 0.7,
           size = 1.5) +
  
  # Error bars
  geom_errorbar(aes(ymin = mean_survival - SE, ymax = mean_survival + SE),
                width = 0.3, size = 1.2, color = "black") +
  
  # Color scales
  scale_fill_manual(values = burn_colors) +
  scale_color_manual(values = burn_colors) +
  
  # Labels
  labs(x = "", y = "Mean proportion of survival (per plot)", 
       fill = "Burn Severity", color = "Burn Severity") +
  
  # Theme
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 35),
    axis.title = element_text(size = 16, face = 'bold'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = 'black')
  )

##establishment##
#establishment 
library(dplyr)
library(tidyr)

# ----------------------------
# 1. Create full plot × year × exclosure framework
# ----------------------------
full_seedlings <- expand_grid(
  Plot = 1:24,
  Year = 2023:2024,
  Exclosure = c("no", "yes")
) %>%
  # Assign burn severity and drought treatment
  mutate(
    BurnTreat = if_else(Plot <= 12, "Moderate_Burn", "Severe_Burn"),
    Drought = if_else(Plot %% 2 == 1, "Drought", "Ambient"),
    n_new_seedlings = 0  # initialize
  ) %>%
  arrange(Plot, Year, Exclosure)

# ----------------------------
# 2. Summarize actual new seedlings from your data
# ----------------------------
additional_seedlings_plot <- shrubs %>%
  filter(Year > 2022) %>%  # only new seedlings
  group_by(Plot, Year, Exclosure) %>%
  summarise(n_new_seedlings = n_distinct(Tag), .groups = "drop")

# 1. Make sure Exclosure is character in both datasets
full_seedlings$Exclosure <- as.character(full_seedlings$Exclosure)
additional_seedlings_plot$Exclosure <- as.character(additional_seedlings_plot$Exclosure)

# 2. Merge using left_join
full_seedlings <- full_seedlings %>%
  left_join(additional_seedlings_plot, 
            by = c("Plot", "Year", "Exclosure"))

# 3. Replace NA in n_new_seedlings with 0
full_seedlings <- full_seedlings %>%
  mutate(
    n_new_seedlings = if_else(
      !is.na(n_new_seedlings.y), 
      n_new_seedlings.y,   # use actual count if present
      0                    # else zero
    )
  ) %>%
  select(-n_new_seedlings.x, -n_new_seedlings.y)  # remove old columns


library(dplyr)

# bring in invasive cover
invasive_cover_long
# Convert columns in invasive_cover_long to match full_seedlings
invasive_cover_long <- invasive_cover_long %>%
  mutate(
    Plot = as.numeric(as.character(Plot)),  # convert factor to numeric
    Year = as.numeric(as.character(Year))   # convert factor to numeric
  )

# Now join
full_seedlings <- full_seedlings %>%
  left_join(invasive_cover_long, by = c("Plot", "Year", "Exclosure"))

# 3. Compute area-corrected density and scaled (z-score)
full_seedlings <- full_seedlings %>%
  mutate(
    plot_area = if_else(Exclosure == "yes", 0.5, 9),      # 0.5 m² for exclosures, 3x3=9 m² for open
    invasion_density = invasive_cover / plot_area,        # stems or cover per m²
    invasion_scaled = scale(invasion_density)            # standardized for modeling
  ) %>%
  arrange(Plot, Year, Exclosure)

library(lme4)

# Convert categorical variables to factors (if not already)
full_seedlings <- full_seedlings %>%
  mutate(
    BurnTreat = factor(BurnTreat),
    Exclosure = factor(Exclosure),
    Drought = factor(Drought)
  )

# Check factor levels and contrasts
levels(full_seedlings$BurnTreat)
contrasts(full_seedlings$BurnTreat)

levels(full_seedlings$Exclosure)
contrasts(full_seedlings$Exclosure)

levels(full_seedlings$invasion_density)
contrasts(full_seedlings$invasion_density)

#anova 
library(dplyr)
library(car)

agg_seedlings <- full_seedlings %>%
  mutate(
    plot_area = ifelse(Exclosure == "yes", 0.5, 9),  # m²
    log_invasive = log1p(invasive_cover)            # log(1 + cover) to handle zeros
  ) %>%
  group_by(Plot, Year, BurnTreat, Exclosure, Drought, plot_area, log_invasive) %>%
  summarise(
    total_seedlings = sum(n_new_seedlings),
    .groups = "drop"
  ) %>%
  mutate(seedling_density = total_seedlings / plot_area)

agg_seedlings <- agg_seedlings %>%
  mutate(
    Year = factor(Year),
    BurnTreat = factor(BurnTreat),
    Exclosure = factor(Exclosure),
    Drought = factor(Drought)
  )

lm_model <- lm(seedling_density ~ Year * BurnTreat * Exclosure * Drought + log_invasive, data = agg_seedlings)

anova_results <- Anova(lm_model, type = "II")  # type II sums-of-squares
anova_results

#time to plot 
# Summarize mean seedling recruitment and SE for plotting
# Summarize across plots for plotting
# Summarize seedling density for plotting
plot_data <- agg_seedlings %>%
  group_by(Year, BurnTreat, Exclosure) %>%
  summarise(
    mean_density = mean(seedling_density),
    se_density = sd(seedling_density)/sqrt(n()),
    .groups = "drop"
  )

#plot 

# Prepare data with bar_group
plot_data_density <- agg_seedlings %>%
  group_by(Year, BurnTreat, Exclosure) %>%
  summarise(
    mean_density = mean(seedling_density),
    SE = sd(seedling_density) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(bar_group = paste(BurnTreat, Exclosure, Year, sep = "_"))


# Reorder bar_group for proper x-axis
plot_data_density$bar_group <- factor(plot_data_density$bar_group,
                                      levels = c(
                                        "Moderate_Burn_no_2023",
                                        "Severe_Burn_no_2023",
                                        "Moderate_Burn_no_2024",
                                        "Severe_Burn_no_2024",
                                        "Moderate_Burn_yes_2023",
                                        "Severe_Burn_yes_2023",
                                        "Moderate_Burn_yes_2024",
                                        "Severe_Burn_yes_2024"
                                      ))

library(ggplot2)
library(dplyr)

# Define burn colors
burn_colors <- c("Moderate_Burn" = "#D88A25", "Severe_Burn" = "cadetblue")


# Plot
ggplot(plot_data_density, aes(x = bar_group, y = mean_density)) +
  
  # Open bars (no exclosure)
  geom_bar(data = plot_data_density %>% filter(Exclosure == "no"),
           aes(color = BurnTreat),
           stat = "identity",
           fill = NA,
           width = 0.7,
           size = 1.5) +
  
  # Filled bars (yes exclosure)
  geom_bar(data = plot_data_density %>% filter(Exclosure == "yes"),
           aes(fill = BurnTreat),
           stat = "identity",
           color = "black",
           width = 0.7,
           size = 1.5) +
  
  # Error bars
  geom_errorbar(aes(ymin = mean_density - SE, ymax = mean_density + SE),
                width = 0.3, size = 1.2, color = "black") +
  
  scale_fill_manual(values = burn_colors) +
  scale_color_manual(values = burn_colors) +
  
  labs(x = "", y = "Mean seedling density (per m²)", 
       fill = "Burn Severity", color = "Burn Severity") +
  
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 45),
    axis.title = element_text(size = 16, face = 'bold'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = 'black')
  )

