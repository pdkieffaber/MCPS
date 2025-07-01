# Load necessary libraries
#install.packages("lme4") # For mixed -effects models 
#install.packages("ggplot2") # For visualization 
#install.packages("lmerTest") # For p-values in mixed models
#library(lme4) 
#library(ggplot2) 
#library(lmerTest)

set.seed(123)
# Set parameters
n_patients <- 10          # Number of patients per therapist
n_therapists <- 5         # Number of therapists
time_points <- c(0, 3, 6) # Baseline, Mid-point, Endpoint

# Total number of observations
n_obs <- n_patients * n_therapists * length(time_points)

# Simulate therapist-level data
therapist_id <- rep(1:n_therapists, each = n_patients * length(time_points) / n_therapists)
therapist_effects <- rep(rnorm(n_therapists, mean = 0, sd = 2), each = n_patients * length(time_points) / n_therapists)

# Simulate patient-level data
therapy_type <- rep(c("CBT", "Mindfulness"), length.out = n_obs)
time <- rep(time_points, times = n_patients * n_therapists)

# Fixed effects: Therapy and time effects
cbt_effect <- ifelse(therapy_type == "CBT", -5, 0)
time_effect <- time * -0.8 # Decreasing anxiety over time
interaction_effect <- ifelse(therapy_type == "CBT", time * -0.8, 0) # Greater improvement for CBT

# Generate anxiety scores
residual_error <- rnorm(n_obs, mean = 0, sd = 5) # Residual error matches n_obs
anxiety_scores <- 50 + cbt_effect + time_effect + interaction_effect + therapist_effects + residual_error

# Create data frame
data <- data.frame(
  anxiety_scores = anxiety_scores,
  therapy_type = factor(therapy_type),
  time = factor(time, levels = time_points),
  therapist_id = factor(therapist_id)
)

# Display summary of the data
summary(data)


# Plot anxiety scores over time by therapy type
ggplot(data, aes(x = time, y = anxiety_scores, color = therapy_type)) + 
    geom_point(position = position_jitter(width = 0.1, height = 0)) + 
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = "Anxiety Scores Over Time by Therapy Type",
         x = "Time (Months)", y = "Anxiety Score") +
    theme_minimal()

# Fit the model
model <- lmer(anxiety_scores ~ therapy_type * time +
                (1 | therapist_id), data = data)
# Display summary
summary(model)
  
  