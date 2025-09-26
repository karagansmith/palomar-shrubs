#evergreen cover data 
library(readr)
evergreen_cover <- read_csv("Desktop/Palamar/NDVI/evergreen_cover2020-2024.csv")
evergreen_cover

#change plot to be capital P 

colnames(evergreen_cover)[colnames(evergreen_cover) == "plot"] <- "Plot"

#treatment data 
treatments <- read_csv("/Users/karagansmith/Desktop/Palamar/Treatments Files/treatments_invasion.csv", col_names = TRUE)

#merge the data 
evergreendf=merge(treatments, evergreen_cover, by='Plot')
library(tidyverse)
#exclude the exclosure 
evergreen <- evergreendf %>%
  select(-Exclosure) %>%  # Remove the Exclosure column
  distinct() 

#check everything 
colnames(evergreendf)

colnames(evergreen_cover)

sum(is.na(evergreendf$Plot))

str(evergreendf)
colnames(evergreen)

# Check the column names after renaming
colnames(evergreen)
#ok now I want to compare the percent cover from each year for each burntreatment 
library(tidyr)

# Reshaping data to long format for year-specific variables
evergreen_long <- evergreen %>%
  pivot_longer(cols = starts_with("evergreen_cover") | starts_with("plot_area") | starts_with("percent_cover"),
               names_to = c("Measurement", "Year"),
               names_pattern = "(.*)_(.*)") %>%
  spread(key = Measurement, value = value)


####CAR ANOVA###

library(car)

model_full <- lm(percent_cover ~ BurnTreat * Year * Drought, data = evergreen_long)

Anova(model_full, type = "III")


#need to do a posthoc 

library(emmeans)

emm <- emmeans(model_full, ~ BurnTreat * Year)

# Tukey-adjusted pairwise comparisons (e.g. comparing all vs. "Moderate Burn 2020")

pairs(emm, adjust = "tukey")


##note BurnedInvaded was renamed to Moderate Burn in text, BurnedNative was reanmed to Severe Burn in text 

# Summarize data
summary_data <- evergreen_long %>%
  group_by(BurnTreat, Drought, Year) %>%
  summarize(
    MeanPercentCover = mean(percent_cover, na.rm = TRUE),
    SDPercentCover = sd(percent_cover, na.rm = TRUE),
    n = n()
  )

# Calculate standard error
summary_data <- evergreen_long %>%
  group_by(BurnTreat, Drought, Year) %>%
  summarize(
    MeanPercentCover = mean(percent_cover, na.rm = TRUE),
    SDPercentCover = sd(percent_cover, na.rm = TRUE),
    SEPercentCover = SDPercentCover / sqrt(n()),
    n = n()
  )


library(ggplot2)

#lets do scatter plot 
library(ggplot2)

# Summarize data to combine Drought and Ambient
summary_combined <- summary_data %>%
  group_by(Year, BurnTreat) %>%
  summarize(
    MeanPercentCover = mean(MeanPercentCover),
    SEPercentCover = sqrt(sum(SEPercentCover^2)) / n(),  # Pooled SE
    .groups = "drop"
  )

# First scatter plot: Combine Drought and Ambient
plot_combined <- ggplot(summary_combined, aes(x = Year, y = MeanPercentCover, color = BurnTreat, group = BurnTreat)) +
  geom_point(size = 3) +  # Points for each year
  geom_line() +           # Lines connecting points for BurnTreat
  geom_errorbar(
    aes(ymin = MeanPercentCover - SEPercentCover, ymax = MeanPercentCover + SEPercentCover),
    width = 0.2
  ) +
  scale_color_manual(values = c(
    "BurnedInvaded" = "#D88A25",
    "BurnedNative" = "cadetblue",
    "Unburned" = "darkseagreen"
  )) +
  labs(
    title = "Percent Evergreen Cover by Year and Burn Treatment (Combined Drought & Ambient)",
    x = "Year",
    y = "Mean Percent Cover"
  ) +
  theme_minimal()

print(plot_combined)

#second scatter plot, drought 
# Second scatter plot: Separate Drought and Ambient
plot_split <- ggplot(summary_data, aes(x = Year, y = MeanPercentCover, color = interaction(BurnTreat, Drought), group = interaction(BurnTreat, Drought))) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +  # Points for each treatment combination
  geom_line(position = position_dodge(width = 0.2)) +            # Lines connecting points for each group
  geom_errorbar(
    aes(ymin = MeanPercentCover - SEPercentCover, ymax = MeanPercentCover + SEPercentCover),
    width = 0.2,
    position = position_dodge(width = 0.2)
  ) +
  scale_color_manual(values = c(
    "BurnedInvaded.Ambient" = "#D88A25",
    "BurnedInvaded.Drought" = "#F4B84C",
    "BurnedNative.Ambient" = "cadetblue",
    "BurnedNative.Drought" = "cadetblue3",
    "Unburned.Ambient" = "darkseagreen",
    "Unburned.Drought" = "lightgreen"
  )) +
  labs(
    title = "Percent Evergreen Cover by Year, Burn Treatment, and Drought",
    x = "Year",
    y = "Mean Percent Cover"
  ) +
  theme_minimal()

print(plot_split)

#make better for publication
library(dplyr)

summary_data <- summary_data %>%
  mutate(TreatmentLabel = case_when(
    BurnTreat == "BurnedInvaded" & Drought == "Ambient" ~ "Moderate Burn Ambient",
    BurnTreat == "BurnedInvaded" & Drought == "Drought" ~ "Moderate Burn Drought",
    BurnTreat == "BurnedNative" & Drought == "Ambient" ~ "Severe Burn Ambient",
    BurnTreat == "BurnedNative" & Drought == "Drought" ~ "Severe Burn Drought",
    BurnTreat == "Unburned" & Drought == "Ambient" ~ "Unburned Ambient",
    BurnTreat == "Unburned" & Drought == "Drought" ~ "Unburned Drought",
    TRUE ~ NA_character_
  ))

# Plot
plot_split <- ggplot(summary_data, aes(x = Year, y = MeanPercentCover, 
                                       color = TreatmentLabel, 
                                       group = TreatmentLabel)) +
  geom_point(size = 12, position = position_dodge(width = 0.2)) +       # larger points
  geom_line(size = 3, position = position_dodge(width = 0.2)) +      # thicker lines
  geom_errorbar(aes(ymin = MeanPercentCover - SEPercentCover,
                    ymax = MeanPercentCover + SEPercentCover),
                width = 0.8,
                size = 2,
                position = position_dodge(width = 0.2)) +                # thicker error bars
  scale_color_manual(values = c(
    "Moderate Burn Ambient" = "#D88A25",
    "Moderate Burn Drought" = "#F4B84C",
    "Severe Burn Ambient" = "cadetblue",
    "Severe Burn Drought" = "cadetblue3",
    "Unburned Ambient" = "darkseagreen",
    "Unburned Drought" = "lightgreen"
  )) +
  labs(
    title = "",
    x = "",
    y = "Mean Evergreen Cover (9 m²)",
    color = ""
  ) +
  theme_minimal(base_size = 14) +             # larger base font size
  theme(
    axis.title = element_text(size = 50, face = "bold"),
    axis.text = element_text(size = 45),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 35),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

plot_split

# Bar plot of percent evergreen cover
plot <- ggplot(summary_data, aes(x = Year, y = MeanPercentCover, fill = BurnTreat)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Adjust bar width if needed
  geom_errorbar(
    aes(ymin = MeanPercentCover - SEPercentCover, ymax = MeanPercentCover + SEPercentCover),
    width = 0.2,
    position = position_dodge(0.7)  # Align error bars with bars
  ) +
  scale_fill_manual(values = c(
    "BurnedInvaded.Ambient" = "#D88A25",   # Dark mustard yellow for Ambient
    "BurnedInvaded.Drought" = "#F4B84C",   # Lighter mustard yellow for Drought
    "BurnedNative.Ambient" = "cadetblue",
    "BurnedNative.Drought" = "cadetblue3",
    "Unburned.Ambient" = "darkseagreen",
    "Unburned.Drought" = "lightgreen"  # Lighter green for Unburned Drought
  )) +
  facet_wrap(~ BurnTreat) +
  labs(
    title = "Percent Evergreen Cover by Year and Treatment",
    x = "Year",
    y = "Mean Percent Cover"
  ) +
  theme_minimal()

# Print the plot
print(plot)


# Print the plot
print(plot)

#Further edited graph in powerpoint for aesthetics

