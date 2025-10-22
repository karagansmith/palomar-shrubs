# Aspect Data 

#load data 

#burned twig data and aspect 

# Define the data w/ the aspect
plot_data <- data.frame(
  plot = 1:24,
  aspect_mean = c(185.8726538, 191.9075836, 185.6372536, 194.3648529, 186.5897935, 
                  197.9801773, 193.6135774, 195.8431396, 193.5862122, 199.0941008, 
                  226.5116919, 227.3507082, 147.3719174, 144.1747742, 134.1131053, 
                  117.3273468, 117.3273468, 96.56177521, 142.9507061, 139.6955763, 
                  121.0239105, 96.56177521, 127.8312728, 133.8134569)
)


# Define the burn treatment and diameter data
burn_data <- data.frame(
  plot = 1:24,
  treatment = c("burneda", "burneda", "burneda", "burneda", "burneda", "burneda", "burneda", 
                "burneda", "burneda", "burneda", "burneda", "burneda", "burnedb", "burnedb", 
                "burnedb", "burnedb", "burnedb", "burnedb", "burnedb", "burnedb", "burnedb", 
                "burnedb", "burnedb", "burnedb"),
  diameter = c(4, 10, 0.5, 5.5, 5, 0.4, 5, 7, 15, 10, 7, 3, 9, 10, 11, 14, 15, 9, 7, 9, 13, 13, 11, 14)
)


# Merge the burn data with the plot data based on plot
plot_data <- merge(plot_data, burn_data, by = "plot")

# View the updated data
head(plot_data)

# Convert treatment to a factor variable (as it's categorical)
plot_data$treatment <- factor(plot_data$treatment, levels = c("burneda", "burnedb"))

# Linear regression (if treatment and diameter are treated as predictors)
regression_model <- lm(diameter ~ aspect_mean, data = plot_data)


#do an ancova 
# Display the regression summary
summary(regression_model)

# Load necessary library
library(ggplot2)

# Create a scatter plot with regression line
ggplot(plot_data, aes(x = aspect_mean, y = diameter)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Regression line without confidence interval
  labs(title = "Relationship between Aspect and Twig Burn Diameter",
       x = "Aspect Mean",
       y = "Twig Burn Diameter (mm)") +
  theme_minimal()

#confidence interval 
ggplot(plot_data, aes(x = aspect_mean, y = diameter)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Confidence interval shown
  labs(title = "Relationship between Aspect and Twig Burn Diameter",
       x = "Aspect Mean",
       y = "Twig Burn Diameter (mm)") +
  theme_minimal()


# Load necessary library
library(ggplot2)

# Create a scatter plot with regression line and custom colors for treatment
ggplot(plot_data, aes(x = aspect_mean, y = diameter, color = treatment)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Regression line (black color)
  scale_color_manual(values = c("burneda" = "goldenrod", "burnedb" = "cadetblue")) +  # Custom colors
  labs(title = "Relationship between Aspect and Twig Burn Diameter",
       x = "Aspect Mean",
       y = "Twig Burn Diameter (mm)") +
  theme_minimal()

#better graph 
# Load necessary library
library(ggplot2)

# Create a scatter plot with larger points, custom axis labels, and custom colors for treatment
ggplot(plot_data, aes(x = aspect_mean, y = diameter, color = treatment)) +
  geom_point(size = 3) +  # Make points larger (adjust size as needed)
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("burneda" = "goldenrod", "burnedb" = "cadetblue")) +  # Custom colors
  scale_x_continuous(name = "Aspect Mean") +  # Label for x-axis
  scale_y_continuous(name = "Twig Burn Diameter (mm)") +  # Label for y-axis
  labs(title = "Relationship between Aspect and Twig Burn Diameter",
       color = "Burn Treatment") +  # Title and legend label
  theme_minimal() +
  scale_color_manual(values = c("burneda" = "goldenrod", "burnedb" = "cadetblue"), 
                     labels = c("Moderate Burn", "Severe Burn"))  # Rename the legend labels


#just burn treatment 
# Box plot of diameter by treatment
# Box plot of diameter by treatment with custom colors and labels
# Box plot of diameter by treatment with custom colors, labels, and x-axis labels
ggplot(burn_data, aes(x = treatment, y = diameter, fill = treatment)) +
  geom_boxplot() + 
  labs(title = "Burned Twig Diameter by Burn Treatment",
       x = "Treatment",
       y = "Diameter (cm)") +
  theme_minimal() +
  scale_fill_manual(
    values = c("burneda" = "goldenrod", "burnedb" = "cadetblue"),
    labels = c("burneda" = "Moderate burn", "burnedb" = "Severe burn")
  ) + 
  scale_x_discrete(
    labels = c("burneda" = "Moderate burn", "burnedb" = "Severe burn")
  )  # Custom x-axis labels

#slope data 

slope_data <- data.frame(
  plot = 1:36,
  QIS_slope = c(
    89.61090308, 89.62802144, 89.60239587, 89.59124698, 89.58836269,
    89.64037171, 89.57756865, 89.54189938, 89.60838721, 89.6432743,
    89.62198331, 89.59667334, 89.64759318, 89.60266445, 89.62356806,
    89.60312794, 89.62356806, 89.60312794, 54.22895056, 40.06818008,
    89.69930197, 89.59097989, 89.63592517, 89.66732104, 89.66276346,
    89.61443489, 89.61618062, 89.61921718, 89.62868013, 89.6116578,
    89.65241105, 89.63569531, 89.65942902, 2.387145757, 89.65085351,
    89.65300565
  ),
  slope = c(
    10.38909692, 10.37197856, 10.39760413, 10.40875302, 10.41163731,
    10.35962829, 10.42243135, 10.45810062, 10.39161279, 10.3567257,
    10.37801669, 10.40332666, 10.35240682, 10.39733555, 10.37643194,
    10.39687206, 10.37643194, 10.39687206, 35.77104944, 30.06818008,
    10.30069803, 10.40902011, 10.36407483, 10.33267896, 10.33723654,
    10.38556511, 10.38381938, 10.38078282, 10.37131987, 10.3883422,
    10.34758895, 10.36430469, 10.34057098, 2.387145757, 10.34914649,
    10.34699435
  )
)

# Display the data
print(slope_data)

# Merge the burn data with the slope data based on plot
slope_burn <- merge(slope_data, burn_data, by = "plot")


# View the updated data
head(slope_burn)

# Convert treatment to a factor variable (as it's categorical)
slope_burn$treatment <- factor(slope_burn$treatment, levels = c("burneda", "burnedb"))

# Linear regression (if treatment and diameter are treated as predictors)
regression_model <- lm(diameter ~ slope, data = slope_burn)

#Display the regression summary
summary(regression_model)

# Load necessary library
library(ggplot2)

# Create a scatter plot with larger points, custom axis labels, and custom colors for treatment
ggplot(slope_burn, aes(x = slope, y = diameter, color = treatment)) +
  geom_point(size = 3) +  # Make points larger (adjust size as needed)
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c("burneda" = "goldenrod", "burnedb" = "cadetblue")) +  # Custom colors
  scale_x_continuous(name = "Slope Mean (degrees)") +  # Label for x-axis
  scale_y_continuous(name = "Twig Burn Diameter (mm)") +  # Label for y-axis
  labs(title = "Relationship between Slope and Twig Burn Diameter",
       color = "Burn Treatment") +  # Title and legend label
  theme_minimal() +
  scale_color_manual(values = c("burneda" = "goldenrod", "burnedb" = "cadetblue"), 
                     labels = c("Moderate Burn", "Severe Burn"))  # Rename the legend labels


