###Biomass Data 2024###

#bring in the data for 2024 
library(readr)
Biomass_2024 <- read_csv("Desktop/Palamar/Biomass Data 2024 - Sheet1.csv", 
                         col_types = cols(
                           `Plot #` = col_double(),
                           Enclosed = col_double(),
                           `Not Enclosed` = col_double()
                         ))

#We need to add in our Treatment types
library(tidyverse)

# Convert to long format and add BurnTreat and Drought columns
Biomass_long <- Biomass_2024 %>%
  pivot_longer(cols = c(Enclosed, `Not Enclosed`), 
               names_to = "Exclosure", 
               values_to = "Biomass") %>%
  mutate(
    BurnTreat = case_when(
      `Plot #` >= 1 & `Plot #` <= 12  ~ "Moderate_Burn",
      `Plot #` >= 13 & `Plot #` <= 24 ~ "Severe_Burn",
      `Plot #` >= 25 & `Plot #` <= 36 ~ "Unburned"
    ),
    Drought = if_else(`Plot #` %% 2 == 0, "Ambient", "Drought")  # Even = Ambient, Odd = Drought
  ) %>%
  rename(Plot = `Plot #`)

#run an Anova to see if there is differences in this year 
#Run ANOVA
anova_model <- aov(Biomass ~ BurnTreat * Drought * Exclosure, data = Biomass_long)

#View Summary 
summary(anova_model)

#Awesome, lets look at posthoc results 

TukeyHSD(anova_model)

#Now lets graph! 

# Calculate mean and standard error for Biomass
Biomass_summary <- Biomass_long %>%
  group_by(BurnTreat, Exclosure) %>%
  summarise(
    Mean_Biomass = mean(Biomass, na.rm = TRUE),
    SE_Biomass = sd(Biomass, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

## Define colors
burn_colors <- c("Moderate_Burn" = "#D88A25",
                 "Severe_Burn" = "cadetblue",
                 "Unburned" = "darkseagreen")

#Plot
# Create bar plot
ggplot(Biomass_summary, aes(x = BurnTreat, y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_summary %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure)),  # Separate bars for each Exclosure type
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  # Black outline for visibility
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_summary %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure), color = BurnTreat),  # Separate bars for each Exclosure type
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  # No fill for enclosed
           linewidth = 1.5) +  # Thick outline for enclosed plots
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure), ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  # Keep legend for fill colors
  scale_color_manual(values = burn_colors, guide = "none") +  # Remove outline legend
  labs(x = NULL, y = "Mean Biomass (g)") +  # Remove x-axis label but keep y-axis
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

#Perfect 

#Ok now lets do 2023 
library(readr)
Palomar_biomass_spring2023 <- read_csv("Desktop/Palamar/Palomar_biomass_spring2023.csv")


#rework this data, we are only interested in the live biomass 
# Transform the data to long format and add necessary columns
library(dplyr)
library(stringr)

Biomass_2023_long <- Palomar_biomass_spring2023 %>%
  # Identify Enclosed vs Not Enclosed from plot name
  mutate(
    Exclosure = if_else(str_detect(Plot, "E"), "Enclosed", "Not Enclosed"),
    Plot = str_remove(Plot, "[OE]")  # Remove 'O' or 'E' suffix from Plot number
  ) %>%
  # Keep relevant columns and rename Live to Biomass
  dplyr::select(Plot, Exclosure, Biomass = Live) %>%
  # Assign burn treatment and drought treatment
  mutate(
    BurnTreat = case_when(
      as.numeric(Plot) >= 1 & as.numeric(Plot) <= 12  ~ "Moderate_Burn",
      as.numeric(Plot) >= 13 & as.numeric(Plot) <= 24 ~ "Severe_Burn",
      as.numeric(Plot) >= 25 & as.numeric(Plot) <= 36 ~ "Unburned"
    ),
    Drought = if_else(as.numeric(Plot) %% 2 == 0, "Ambient", "Drought")
  )

# Run the ANOVA
anova_result <- aov(Biomass ~ BurnTreat * Exclosure * Drought, data = Biomass_2023_long)

# Summary of ANOVA
summary(anova_result)

# Tukey HSD post-hoc test for BurnTreat
tukey_result <- TukeyHSD(anova_result, "BurnTreat")

# Print the results
summary(tukey_result)

# Summary statistics for biomass (mean and SE)
Biomass_summary_2023 <- Biomass_2023_long %>%
  group_by(BurnTreat, Exclosure) %>%
  summarise(
    Mean_Biomass = mean(Biomass, na.rm = TRUE),
    SE_Biomass = sd(Biomass, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# Define colors for BurnTreat
burn_colors <- c("Moderate_Burn" = "#D88A25",
                 "Severe_Burn" = "cadetblue",
                 "Unburned" = "darkseagreen")

# Create the bar plot
ggplot(Biomass_summary_2023, aes(x = BurnTreat, y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed
  geom_bar(data = Biomass_summary_2023 %>% filter(Exclosure == "Not Enclosed"), 
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +
  # Outlined bars for Enclosed
  geom_bar(data = Biomass_summary_2023 %>% filter(Exclosure == "Enclosed"), 
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white", 
           color = "black", 
           linewidth = 1.5) +
  # Error bars
  geom_errorbar(aes(ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors) +
  labs(x = NULL, y = "Mean Biomass (g)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank() 
  )

ggplot(Biomass_summary_2023, aes(x = BurnTreat, y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_summary_2023 %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure)),  # Separate bars for each Exclosure type
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  # Black outline for visibility
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_summary_2023 %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure), color = BurnTreat),  # Separate bars for each Exclosure type
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  # No fill for enclosed
           linewidth = 1.5) +  # Thick outline for enclosed plots
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure), ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  # Keep legend for fill colors
  scale_color_manual(values = burn_colors, guide = "none") +  # Remove outline legend
  labs(x = NULL, y = "Mean Biomass (g)") +  # Remove x-axis label but keep y-axis
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )
#Great! 

# Combine both years' summary data
Biomass_summary_2023$Year <- "2023"
Biomass_summary$Year <- "2024"

Biomass_combined <- bind_rows(Biomass_summary_2023, Biomass_summary)

# Define colors for BurnTreat
burn_colors <- c("Moderate_Burn" = "#D88A25",
                 "Severe_Burn" = "cadetblue",
                 "Unburned" = "darkseagreen")

# Create the combined bar plot
ggplot(Biomass_combined, aes(x = interaction(BurnTreat, Exclosure), 
                             y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year, color = BurnTreat),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  
           linewidth = 1.5) +  
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure, Year), 
                    ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  
  scale_color_manual(values = burn_colors, guide = "none") +  
  labs(x = NULL, y = "Mean Biomass (g)", fill = "Burn Treatment") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

# Create the combined bar plot without grid lines
ggplot(Biomass_combined, aes(x = interaction(BurnTreat, Exclosure), 
                             y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year, color = BurnTreat),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  
           linewidth = 1.5) +  
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure, Year), 
                    ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  
  scale_color_manual(values = burn_colors, guide = "none") +  
  labs(x = NULL, y = "Mean Biomass (g)", fill = "Burn Treatment") +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

# Create the combined bar plot with larger font and no grid lines
ggplot(Biomass_combined, aes(x = interaction(BurnTreat, Exclosure), 
                             y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year, color = BurnTreat),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  
           linewidth = 1.5) +  
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure, Year), 
                    ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  
  scale_color_manual(values = burn_colors, guide = "none") +  
  labs(x = NULL, y = "Mean Biomass (g)", fill = "Burn Treatment") +  
  theme_minimal(base_size = 16) +  # Set base font size
  theme(
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.line = element_line(color = "black", linewidth = 0.8),  # Add axis lines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # X axis labels
    axis.text.y = element_text(size = 16),  # Y axis labels
    axis.title.y = element_text(size = 18),  # Y axis title
    legend.title = element_text(size = 18),  # Legend title
    legend.text = element_text(size = 16)    # Legend text
  )
# Create the combined bar plot with larger font, no grid lines, and no x-axis labels
ggplot(Biomass_combined, aes(x = interaction(BurnTreat, Exclosure), 
                             y = Mean_Biomass, fill = BurnTreat)) +
  # Open (outlined) bars for Not Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year, color = BurnTreat),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  
           linewidth = 2) +  
  # Filled bars for Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure, Year), 
                    ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.5, 
                position = position_dodge(width = 0.9), size = 2) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  
  scale_color_manual(values = burn_colors, guide = "none") +  
  labs(x = NULL, y = "Mean Biomass (g)", fill = "Burn Treatment") +  
  theme_minimal(base_size = 16) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text.x = element_blank(),     # Remove x-axis labels
    axis.ticks.x = element_blank(),    # Remove x-axis ticks
    axis.text.y = element_text(size = 55),
    axis.title.y = element_text(size = 60, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

#make better for publication 

# Create the combined bar plot with cleaned y-axis and no x-axis labels
ggplot(Biomass_combined, aes(x = interaction(BurnTreat, Exclosure), 
                             y = Mean_Biomass, fill = BurnTreat)) +
  # Filled bars for Not Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Not Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black") +  
  # Outlined bars for Enclosed plots
  geom_bar(data = Biomass_combined %>% filter(Exclosure == "Enclosed"), 
           aes(x = interaction(BurnTreat, Exclosure, Year), group = Year, color = BurnTreat),  
           stat = "identity", 
           position = position_dodge(width = 0.8), 
           fill = "white",  
           linewidth = 1.5) +  
  # Error bars
  geom_errorbar(aes(x = interaction(BurnTreat, Exclosure, Year), 
                    ymin = Mean_Biomass - SE_Biomass, ymax = Mean_Biomass + SE_Biomass), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = burn_colors, name = "Burn Treatment") +  
  scale_color_manual(values = burn_colors, guide = "none") +  
  scale_y_continuous(expand = c(0, 0)) +  # Prevent y-axis from starting below zero
  labs(x = NULL, y = "Mean Biomass (g)", fill = "Burn Treatment") +  
  theme_minimal(base_size = 16) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 40),
    axis.title.y = element_text(size = 55),
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 50)
  )

#stats ! 

library(lme4)

library(dplyr)
library(tidyr)
library(stringr)

library(dplyr)
library(tidyr)
library(stringr)

# 2024 biomass: pivot to long and annotate metadata
Biomass_2024_long <- Biomass_2024 %>%
  pivot_longer(cols = c(Enclosed, `Not Enclosed`), 
               names_to = "Exclosure", 
               values_to = "Biomass") %>%
  mutate(
    BurnTreat = case_when(
      `Plot #` >= 1 & `Plot #` <= 12  ~ "Moderate_Burn",
      `Plot #` >= 13 & `Plot #` <= 24 ~ "Severe_Burn",
      `Plot #` >= 25 & `Plot #` <= 36 ~ "Unburned"
    ),
    Drought = if_else(`Plot #` %% 2 == 0, "Ambient", "Drought"),
    Plot = as.character(`Plot #`),
    Year = "2024"
  ) %>%
  dplyr::select(Plot, Exclosure, Biomass, BurnTreat, Drought, Year)


# 2023 biomass: clean and annotate
Biomass_2023_long <- Palomar_biomass_spring2023 %>%
  mutate(
    Exclosure = if_else(str_detect(Plot, "E"), "Enclosed", "Not Enclosed"),
    Plot = str_remove(Plot, "[OE]"),
    Biomass = Live,
    BurnTreat = case_when(
      as.numeric(Plot) >= 1 & as.numeric(Plot) <= 12  ~ "Moderate_Burn",
      as.numeric(Plot) >= 13 & as.numeric(Plot) <= 24 ~ "Severe_Burn",
      as.numeric(Plot) >= 25 & as.numeric(Plot) <= 36 ~ "Unburned"
    ),
    Drought = if_else(as.numeric(Plot) %% 2 == 0, "Ambient", "Drought"),
    Year = "2023"
  ) %>%
  dplyr::select(Plot, Exclosure, Biomass, BurnTreat, Drought, Year)

# Combine both years
Biomass_combined <- bind_rows(Biomass_2023_long, Biomass_2024_long)

# Load required package
library(lme4)
install.packages("lmerTest")
library(lmerTest)  # for p-values

# Fit the model: random effect for Plot
biomass_lmm <- lmer(Biomass ~ BurnTreat * Exclosure * Year + (1 | Plot), data = Biomass_combined)


# Ensure proper contrasts for Type III ANOVA
options(contrasts = c("contr.sum", "contr.poly"))
# Fit the model
biomass_lmm <- lmer(Biomass ~ BurnTreat * Exclosure * Year * Drought + (1 | Plot), data = Biomass_combined)

emmeans(biomass_lmm, pairwise ~ BurnTreat * Exclosure* Year * Drought)


# Run Type III ANOVA
Anova(biomass_lmm, type = 3)


# View the summary with p-values
summary(biomass_lmm)

