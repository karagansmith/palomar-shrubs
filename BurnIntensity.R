#this is for burn intensity w/ the Palomar data set 

#bring in the treatments 
library(readr)
pal_college_twigd_csv <- read_csv("~/Desktop/Palamar/Burn Intensity/pal_college_twigd_csv.csv")
View(pal_college_twigd_csv)
library(dplyr)

#transfrom burneda to BurnedInvaded and burnedb to BurnedNative 
# Rename columns
pal_college_twigd_csv <- pal_college_twigd_csv %>%
  mutate(treatment = case_when(
    treatment == "burneda" ~ "BurnedInvaded",
    treatment == "burnedb" ~ "BurnedNative",
    TRUE ~ treatment  # Keep other values unchanged
  ))

# Subset data for the two treatment groups you want to compare
group1 <- pal_college_twigd_csv$diameter[pal_college_twigd_csv$treatment == "BurnedInvaded"]
group2 <- pal_college_twigd_csv$diameter[pal_college_twigd_csv$treatment != "BurnedInvaded"]

# Run t-test
ttest_result <- t.test(group1, group2)

# Print the results
print(ttest_result)


#now boxplot 
library(ggplot2)

# Assuming you have already transformed the treatment column in your dataframe

# Create a boxplot
ggplot(pal_college_twigd_csv, aes(x = treatment, y = diameter, fill = treatment)) +
  geom_boxplot() +
  labs(title = "",
       x = " Burn Treatment",
       y = "Diameter (mm)") +
  theme_minimal() +  
  theme(text = element_text(color = "black", size = 15),  # Darker and larger text
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),  # Larger x-axis text
        axis.text.y = element_text(size = 12),  # Larger y-axis text
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +  # Remove both minor and major grid lines
  scale_fill_manual(values = c("BurnedInvaded" = "coral3",
                               "BurnedNative" = "cadetblue"))  # Specify fill colors
