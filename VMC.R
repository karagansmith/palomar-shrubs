#load the data 
library(readxl)
WeeklyProfilesNov2022_May2024 <- read_excel("Desktop/Palamar/WeeklyProfilesNov2022-May2024.xlsx")
View(WeeklyProfilesNov2022_May2024)

#put into long format 
# Convert to long format
vwc_long <- WeeklyProfilesNov2022_May2024 %>%
  pivot_longer(cols = -UTCdate, 
               names_to = "Plot_Depth", 
               values_to = "VWC") %>%
  separate(Plot_Depth, into = c("VWC", "Plot", "Depth"), sep = "_") %>%
  mutate(
    Plot = as.integer(Plot),  # Convert Plot to numeric
    BurnTreat = case_when(
      Plot >= 1 & Plot <= 12 ~ "Moderate_Burn",
      Plot >= 13 & Plot <= 24 ~ "Severe_Burn",
      TRUE ~ NA_character_
    ),
    Drought = if_else(Plot %% 2 == 1, "Drought", "Ambient"),  # Odd = Drought, Even = Ambient
    Plot = as.factor(Plot),  # Convert back to factor for plotting
    Depth = factor(Depth, levels = c("a", "b", "c"))  # Ensure correct depth order
  )

library(tidyr)
library(lubridate)

# Convert to long format and add treatments
vwc_long <- WeeklyProfilesNov2022_May2024 %>%
  pivot_longer(cols = -UTCdate, 
               names_to = "Plot_Depth", 
               values_to = "VWC") %>%
  # Split Plot_Depth into Plot and Depth correctly
  separate(Plot_Depth, into = c("VWC_temp", "Plot", "Depth"), sep = "_") %>%
  mutate(
    Plot = as.integer(Plot),  # Convert Plot to numeric for treatment assignment
    BurnTreat = case_when(
      Plot >= 1 & Plot <= 12 ~ "Moderate_Burn",
      Plot >= 13 & Plot <= 24 ~ "Severe_Burn",
      Plot >= 25 & Plot <= 36 ~ "Unburned",
      TRUE ~ NA_character_
    ),
    Drought = case_when(
      Plot %% 2 == 1 ~ "Drought",
      Plot %% 2 == 0 ~ "Ambient",
      TRUE ~ NA_character_
    ),
    Plot = as.factor(Plot),  # Convert Plot back to factor for plotting
    Depth = factor(Depth, levels = c("a", "b", "c")),  # Ensure correct depth order
    UTCdate = as_date(UTCdate)  # Keep only the date
  ) %>%
  select(-VWC_temp)  # Remove the temporary VWC column used during separation


library(dplyr)

# Function to calculate standard error
calc_se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

# Check for non-numeric values in VWC
unique(vwc_long$VWC[!grepl("^-?\\d*(\\.\\d+)?$", vwc_long$VWC)])

# Check for non-numeric values in the VWC column
non_numeric_values <- vwc_long %>% 
  filter(!grepl("^-?\\d*(\\.\\d+)?$", VWC)) %>%  # Filter non-numeric entries
  select(VWC) %>% 
  distinct()

# Summarize VWC by BurnTreat, Drought, and Depth for each UTCdate
vwc_summary <- vwc_long %>%
  mutate(Depth = ifelse(is.na(Depth), "profile", as.character(Depth))) %>%  # Handle NA Depth as 'profile'
  group_by(UTCdate, BurnTreat, Drought, Depth) %>%
  summarise(
    Mean_VWC = mean(VWC, na.rm = TRUE),  # Calculate mean VWC
    SE_VWC = sd(VWC, na.rm = TRUE) / sqrt(n()),  # Calculate standard error of the VWC
    .groups = "drop"  # Drop grouping after summarization
  )

vwc_summary <- vwc_summary %>%
  mutate(
    Treatment = paste(BurnTreat, Drought, sep = "."),
    Depth = ifelse(is.na(Depth), "profile", as.character(Depth))  # Handle NA Depth as 'profile'
  )
# Define the color and shape mapping
color_mapping <- c(
  "Moderate_Burn.Ambient" = "darkorange",  # Dark orange for Ambient
  "Moderate_Burn.Drought" = "goldenrod",   # Goldenrod for Drought
  "Severe_Burn.Ambient" = "skyblue",      # Light blue for Ambient
  "Severe_Burn.Drought" = "cadetblue",    # Cadet blue for Drought
  "Unburned.Ambient" = "lightgreen",      # Light green for Ambient
  "Unburned.Drought" = "darkseagreen"     # Dark seagreen for Drought
)
shape_mapping <- c(
  "Ambient" = 16,  # Solid circle for Ambient
  "Drought" = 17   # Triangle for Drought
)

# Create the plot for each depth
# Create the plot with custom colors and shapes
ggplot(vwc_summary, aes(x = UTCdate, y = Mean_VWC, color = Treatment, shape = Drought)) +
  geom_point(alpha = 0.7) +  # Add points with transparency
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), width = 0.2) +  # Add error bars
  facet_wrap(~ Depth, scales = "free_y") +  # Facet by depth, with independent y scales
  labs(x = "Date", y = "Volumetric Water Content (VWC)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  scale_color_manual(values = color_mapping) +  # Apply custom colors
  scale_shape_manual(values = shape_mapping)  # Apply custom shapes

#yearly average 
library(dplyr)
library(lubridate)

# Extract the year from UTCdate
vwc_yearly_avg <- vwc_long %>%
  filter(!is.na(VWC)) %>%  # Ensure no NA values
  mutate(Year = year(UTCdate)) %>%  # Extract the year from the UTCdate
  group_by(Year, BurnTreat, Drought, Depth) %>%
  summarise(
    Mean_VWC = mean(VWC, na.rm = TRUE),
    SE_VWC = calc_se(VWC),  # Calculate SE for VWC if needed
    .groups = "drop"
  )


#new yearly average based on rainy season 
vwc_yearly_avg <- vwc_long %>%
  filter(!is.na(VWC)) %>%  # Ensure no NA values
  mutate(
    Water_Year = case_when(
      UTCdate >= as.Date("2022-04-01") & UTCdate < as.Date("2023-05-01") ~ "2022-2023",
      UTCdate >= as.Date("2023-05-01") & UTCdate < as.Date("2024-04-30") ~ "2023-2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Water_Year)) %>%  # Remove anything outside defined water years
  group_by(Water_Year, BurnTreat, Drought, Depth) %>%
  summarise(
    Mean_VWC = mean(VWC, na.rm = TRUE),
    SE_VWC = sd(VWC, na.rm = TRUE) / sqrt(n()),  # Standard error calculation
    .groups = "drop"
  )

vwc_yearly_avg <- vwc_yearly_avg %>%
  mutate(
    Treatment = paste(BurnTreat, Drought, sep = "."),
    Depth = ifelse(is.na(Depth), "profile", as.character(Depth))  # Handle NA Depth as 'profile'
  )

library(ggplot2)

# Create the box plot
ggplot(vwc_yearly_avg, aes(x = interaction(BurnTreat, Drought), y = Mean_VWC, fill = interaction(BurnTreat, Drought))) +
  geom_boxplot() +  # Box plot
  facet_wrap(~ Depth, scales = "free_y") +  # Facet by Depth
  labs(x = "Burn Treatment and Drought", y = "Yearly Average VWC (Volumetric Water Content)", title = "VWC Box Plot by Depth, Burn Treatment, and Drought") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

#by year 
# Create the box plot for each year
ggplot(vwc_yearly_avg, aes(x = interaction(BurnTreat, Drought), y = Mean_VWC, fill = interaction(BurnTreat, Drought))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph with dodge position
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                position = position_dodge(0.7), width = 0.25) +  # Error bars for SE
  facet_wrap(~ Year + Depth, scales = "free_y") +  # Facet by Year and Depth with independent y-axis for each Depth
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Depth, Burn Treatment, and Drought") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

ggplot(vwc_yearly_avg, aes(x = interaction(BurnTreat, Drought), y = Mean_VWC, fill = interaction(BurnTreat, Drought))) +
  geom_boxplot() +  # Box plot
  facet_wrap(~ Depth + Year, scales = "free_y") +  # Facet by Depth and Year, independent y-axis for each depth
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Box Plot by Depth, Burn Treatment, and Drought") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )
ggplot(vwc_yearly_avg, aes(x = interaction(BurnTreat, Drought), y = Mean_VWC, fill = interaction(BurnTreat, Drought))) +
  geom_boxplot() +  # Box plot
  facet_wrap( ~ Year + Depth, scales = "free_y") +  # Separate by Year and Depth, with independent y-axis
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Box Plot by Depth, Burn Treatment, and Drought") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

ggplot(vwc_yearly_avg, aes(x = interaction(BurnTreat, Drought), y = Mean_VWC, fill = interaction(BurnTreat, Drought))) +
  geom_boxplot() +  # Box plot
  facet_wrap(~ Depth + Year, scales = "free_y") +  # Separate by Depth and Year with independent y-axis
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Box Plot by Depth, Burn Treatment, and Drought") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

library(ggplot2)
# a depth 
vwc_yearly_avg %>%
  filter(Depth == "a") %>%  # Only keep rows with Depth 'a'
  ggplot(aes(x = factor(interaction(BurnTreat, Drought), 
                        levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                                   "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                                   "Unburned.Ambient", "Unburned.Drought")),
             y = Mean_VWC, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                width = 0.25, position = position_dodge(0.7)) +  # Error bars
  facet_wrap(~ Water_Year, scales = "fixed") +  # Same y-axis scale for all years
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Burn Treatment and Drought (Depth a)") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

#b 
vwc_yearly_avg %>%
  filter(Depth == "b") %>%  # Only keep rows with Depth 'a'
  ggplot(aes(x = factor(interaction(BurnTreat, Drought), 
                        levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                                   "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                                   "Unburned.Ambient", "Unburned.Drought")),
             y = Mean_VWC, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                width = 0.25, position = position_dodge(0.7)) +  # Error bars
  facet_wrap(~ Water_Year, scales = "fixed") +  # Same y-axis scale for all years
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Burn Treatment and Drought (Depth b)") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

#c
vwc_yearly_avg %>%
  filter(Depth == "c") %>%  # Only keep rows with Depth 'a'
  ggplot(aes(x = factor(interaction(BurnTreat, Drought), 
                        levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                                   "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                                   "Unburned.Ambient", "Unburned.Drought")),
             y = Mean_VWC, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                width = 0.25, position = position_dodge(0.7)) +  # Error bars
  facet_wrap(~ Water_Year, scales = "fixed") +  # Same y-axis scale for all years
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Burn Treatment and Drought (Depth c)") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

#profile 
vwc_yearly_avg %>%
  filter(Depth == "profile") %>%  # Only keep rows with Depth 'a'
  ggplot(aes(x = factor(interaction(BurnTreat, Drought), 
                        levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                                   "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                                   "Unburned.Ambient", "Unburned.Drought")),
             y = Mean_VWC, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                width = 0.25, position = position_dodge(0.7)) +  # Error bars
  facet_wrap(~ Water_Year, scales = "fixed") +  # Same y-axis scale for all years
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Burn Treatment and Drought (Profile)") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )


#lets just stick with this one 
vwc_yearly_avg %>%
  filter(Depth == "profile") %>%  # Only keep rows with Depth 'a'
  ggplot(aes(x = factor(interaction(BurnTreat, Drought), 
                        levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                                   "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                                   "Unburned.Ambient", "Unburned.Drought")),
             y = Mean_VWC, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar graph
  geom_errorbar(aes(ymin = Mean_VWC - SE_VWC, ymax = Mean_VWC + SE_VWC), 
                width = 0.25, position = position_dodge(0.7)) +  # Error bars
  facet_wrap(~ Water_Year, scales = "fixed") +  # Same y-axis scale for all years
  labs(x = "Burn Treatment and Drought", y = "Mean VWC (Volumetric Water Content)", 
       title = "Yearly VWC Bar Plot by Burn Treatment and Drought (Profile)") +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Rotate x-axis labels for readability
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12)
  )

#can I make a stats table for this? 

library(car)
library(dplyr)

# Use the raw plot-level data (averaged across depths per plot/date)
vwc_plot_avg <- vwc_long %>%
  group_by(UTCdate, Plot, BurnTreat, Drought) %>%
  summarise(VWC_profile = mean(VWC, na.rm = TRUE), .groups = "drop") %>%
  mutate(Water_Year = if_else(lubridate::month(UTCdate) >= 10,
                              lubridate::year(UTCdate) + 1,
                              lubridate::year(UTCdate)))

# Fit a linear model with interactions
mod <- lm(VWC_profile ~ BurnTreat * Drought * Water_Year, data = vwc_plot_avg)

# ANOVA table using car
car::Anova(mod, type = 3)   # type = 3 if you want to look at interactions carefully

vwc_yearly_avg %>%
  filter(Depth == "profile") %>%
  ggplot(aes(
    x = factor(interaction(BurnTreat, Drought), 
               levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                          "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                          "Unburned.Ambient", "Unburned.Drought")),
    y = VWC,   # <-- Use raw values, not Mean_VWC
    fill = interaction(BurnTreat, Drought)
  )) +
  geom_boxplot(width = 0.7, outlier.shape = 21, outlier.alpha = 0.6) +
  facet_wrap(~ Water_Year, scales = "fixed") +
  labs(
    x = "Burn Treatment and Drought", 
    y = "Volumetric Water Content (VWC)", 
    title = "Yearly VWC Boxplot by Burn Treatment and Drought (Profile)"
  ) +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12)
  )

library(lubridate)
library(dplyr)

library(ggplot2)



library(lubridate)
library(dplyr)
library(ggplot2)

vwc_long %>%
  filter(Depth == "profile") %>%   # if "profile" exists in Depth
  mutate(Water_Year = if_else(month(UTCdate) >= 10, 
                              year(UTCdate) + 1, 
                              year(UTCdate))) %>%  # WY starts in Oct
  ggplot(aes(
    x = factor(interaction(BurnTreat, Drought), 
               levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                          "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                          "Unburned.Ambient", "Unburned.Drought")),
    y = VWC,
    fill = interaction(BurnTreat, Drought)
  )) +
  geom_boxplot(width = 0.7, outlier.shape = 21, outlier.alpha = 0.6) +
  facet_wrap(~ Water_Year, scales = "fixed") +
  labs(
    x = "Burn Treatment and Drought", 
    y = "Volumetric Water Content (VWC)", 
    title = "Yearly VWC Boxplot by Burn Treatment and Drought (Profile)"
  ) +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12)
  )

library(lubridate)
library(dplyr)

library(ggplot2)

vwc_long %>%
  # filter(Depth == "profile") %>%   # remove this for now
  mutate(Water_Year = if_else(month(UTCdate) >= 10, 
                              year(UTCdate) + 1, 
                              year(UTCdate))) %>%
  ggplot(aes(
    x = factor(interaction(BurnTreat, Drought), 
               levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought", 
                          "Severe_Burn.Ambient", "Severe_Burn.Drought", 
                          "Unburned.Ambient", "Unburned.Drought")),
    y = VWC,
    fill = interaction(BurnTreat, Drought)
  )) +
  geom_boxplot(width = 0.7, outlier.shape = 21, outlier.alpha = 0.6) +
  facet_wrap(~ Water_Year, scales = "fixed") +
  labs(
    x = "Burn Treatment and Drought", 
    y = "Volumetric Water Content (VWC)", 
    title = "Yearly VWC Boxplot by Burn Treatment and Drought"
  ) +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25", 
    "Moderate_Burn.Drought" = "#F4B84C", 
    "Severe_Burn.Ambient" = "cadetblue", 
    "Severe_Burn.Drought" = "cadetblue3", 
    "Unburned.Ambient" = "darkseagreen", 
    "Unburned.Drought" = "lightgreen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12)
  )

#summarise 
library(dplyr)
library(ggplot2)
library(lubridate)

vwc_plot_avg <- vwc_long %>%
  group_by(UTCdate, Plot, BurnTreat, Drought) %>%
  summarise(VWC_profile = mean(VWC, na.rm = TRUE), .groups = "drop") %>%
  mutate(Water_Year = if_else(month(UTCdate) >= 10,
                              year(UTCdate) + 1,
                              year(UTCdate)))

ggplot(vwc_plot_avg, aes(
  x = factor(interaction(BurnTreat, Drought),
             levels = c("Moderate_Burn.Ambient", "Moderate_Burn.Drought",
                        "Severe_Burn.Ambient", "Severe_Burn.Drought",
                        "Unburned.Ambient", "Unburned.Drought")),
  y = VWC_profile,
  fill = interaction(BurnTreat, Drought)
)) +
  geom_boxplot(width = 0.7, outlier.shape = 21, outlier.alpha = 0.6) +
  facet_wrap(~ Water_Year, scales = "fixed") +
  labs(
    x = "Burn Treatment and Drought",
    y = "Volumetric Water Content (VWC)",
    title = "Yearly VWC Boxplot by Burn Treatment and Drought (Plot-level)"
  ) +
  scale_fill_manual(values = c(
    "Moderate_Burn.Ambient" = "#D88A25",
    "Moderate_Burn.Drought" = "#F4B84C",
    "Severe_Burn.Ambient" = "cadetblue",
    "Severe_Burn.Drought" = "cadetblue3",
    "Unburned.Ambient" = "darkseagreen",
    "Unburned.Drought" = "lightgreen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12)
  )


