# Econ 26610 Homework 2
# Jackson Van Vooren, Khwaish Vohra, Katherine Chen

# Load packages
library(stargazer)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data
data_pre <- read.csv("/Users/jacksonvanvooren/Downloads/data/data_pre.csv")
data_post <- read.csv("/Users/jacksonvanvooren/Downloads/data/data_post.csv")
data_fundingcenters <- read.csv("/Users/jacksonvanvooren/Downloads/data/data_fundingcenters.csv")

#------------------------------------------------------------------------------#

# 2
# Assign categories (useful for LOESS smoothing later)
data_pre$policy_period <- "Pre-Policy"
data_post$policy_period <- "Post-Policy"

# Merge and analyze data
merged_data <- rbind(data_pre, data_post)
prop.table(table(merged_data$heating))
mean(merged_data$age)
mean(merged_data$landarea)

#------------------------------------------------------------------------------#

# 3
# Pre policy
# Assign base categories
df_pre <- data_pre %>%
  mutate(
    year = relevel(as.factor(year), ref = "1998"),
    exterior = relevel(as.factor(exterior), ref = "Wood or other"),
    heating = relevel(as.factor(heating), ref = "Gas"),
    condition = relevel(as.factor(condition), ref = "Good")
  )
df_pre <- df_pre %>%
  mutate(centralac = ifelse(centralac == 1, 1, 0))

# Extract dependend variable
y <- df_pre$lpricesqft  

# Dummies
X <- model.matrix(~ year + exterior + heating + condition + livingarea + age +
                    landarea + bath + centralac, data = df_pre) %>%
  as.data.frame()

# Regress
model_pre <- lm(y ~ . - 1, data = X)
summary(model_pre)


# Post policy
# Assign base categories
df_post <- data_post %>%
  mutate(
    year = relevel(as.factor(year), ref = "2004"),
    exterior = relevel(as.factor(exterior), ref = "Wood or other"),
    heating = relevel(as.factor(heating), ref = "Gas"),
    condition = relevel(as.factor(condition), ref = "Good")
  )
df_post <- df_post %>%
  mutate(centralac = ifelse(centralac == 1, 1, 0))

# Extract dependend variable
y <- df_post$lpricesqft  

# Dummies
X <- model.matrix(~ year + exterior + heating + condition + livingarea + age +
                    landarea + bath + centralac, data = df_post) %>%
  as.data.frame()

# Regress
model_post <- lm(y ~ . - 1, data = X)
summary(model_post)

# Make Latex tables
stargazer(model_pre, title="Pre-Policy Results", align=TRUE)
stargazer(model_post, title="Pre-Policy Results", align=TRUE)

#------------------------------------------------------------------------------#

# 5
# Initialize list and clean data to relevant neighborhoods
hist_plots <- list()
df_merged_cleaned <- merged_data %>%
  filter(!is.na(neighborhood)) %>%
  filter(neighborhood %in% data_fundingcenters$neighborhood)

# Helper to calculate Euclidean distance
calculate_euclidean <- function(x1, x2, y1, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

unique_neighborhoods <- unique(df_merged_cleaned$neighborhood)
min_distances <- numeric(nrow(df_merged_cleaned))

# Loop through neighborhoods with funding centers
for (neigh in unique_neighborhoods) {
  df_homes_neigh <- df_merged_cleaned %>% filter(neighborhood == neigh)
  df_funding_neigh <- data_fundingcenters %>% filter(neighborhood == neigh)

  neighborhood_distances <- numeric(nrow(df_homes_neigh))

  # Loop through homes in the corresponding neighborhood
  for (i in 1:nrow(df_homes_neigh)) {
    home_row <- df_homes_neigh[i, ]
    home_distances <- numeric(nrow(df_funding_neigh))
    
    # Find all distances to the funding center(s) for each home
    for (j in 1:nrow(df_funding_neigh)) {
      funding_row <- df_funding_neigh[j, ]
      distance <- calculate_euclidean(home_row$geo_x, funding_row$geo_x, 
                                      home_row$geo_y, funding_row$geo_y)
      home_distances[j] <- distance
    }
    
    # Take min distance for each home
    neighborhood_distances[i] <- min(home_distances)
  }
  
  # Average min distance for neighborhood
  cat(sprintf("%s average: %.2f\n", neigh, mean(neighborhood_distances)))
  
  # Update min_distances
  min_distances[df_merged_cleaned$neighborhood == neigh] <- neighborhood_distances
  
  # Plot histogram for each neighborhood
  p <- ggplot(data.frame(distance = neighborhood_distances), aes(x = distance)) +
    geom_histogram(binwidth = 50, color = "black", fill = "blue") +
    labs(title = paste("Minimum Distances for", neigh),
         x = "Distance", y = "Frequency") +
    theme_minimal()
  hist_plots[[neigh]] <- p
}

# Update df_merged_cleaned with the correct min_distance values
df_merged_cleaned$min_distance <- min_distances

# Total average
cat(sprintf("Total average: %.2f\n", mean(df_merged_cleaned$min_distance)))

# Plot histograms together in one figure
grid.arrange(grobs = hist_plots, ncol = 2)

#------------------------------------------------------------------------------#

# 6
# Plot using geom_smooth LOESS
p <- ggplot(df_merged_cleaned, aes(x = min_distance, y = lpricesqft,
                                   color = policy_period)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Log Land Price per Sqft vs. Distance to Funding Center",
       x = "Distance to Closest Funding Center",
       y = "Log Land Price per Sqft") +
  xlim(0, 1500) +
  theme_minimal() +
  scale_color_manual(values = c("Pre-Policy" = "blue", "Post-Policy" = "red")) +
  theme(legend.title = element_blank()) +
  facet_wrap(~neighborhood)

print(p)
