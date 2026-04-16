# Load necessary libraries
library(tidyverse)

# Import the data
dat <- read_csv("/Users/paul/Downloads/PsyToolkitData_DRM_InClass_2026_04_16_13_36/data.csv")

#create proportion scores
  # hits ranges from 0 to 8
  # lure_fa ranges from 0 to 4
  # foil_fa ranges from 0 to 8
dat <- dat %>%
  mutate(
    hit_rate = hits / 8,
    lure_fa_rate = lure_fa / 4,
    foil_fa_rate = foil_fa / 8
  )

# Get a quick descriptive summary
summary_rates <- dat %>%
  summarise(
    mean_hit = mean(hit_rate, na.rm = TRUE),
    sd_hit = sd(hit_rate, na.rm = TRUE),
    mean_lure_fa = mean(lure_fa_rate, na.rm = TRUE),
    sd_lure_fa = sd(lure_fa_rate, na.rm = TRUE),
    mean_foil_fa = mean(foil_fa_rate, na.rm = TRUE),
    sd_foil_fa = sd(foil_fa_rate, na.rm = TRUE)
  )

summary_rates

# Plot the main results
plot_dat <- dat %>%
  select(hit_rate, lure_fa_rate, foil_fa_rate) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "rate") %>%
  mutate(
    measure = factor(
      measure,
      levels = c("hit_rate", "lure_fa_rate", "foil_fa_rate"),
      labels = c("Studied words (hits)", "Critical lures (false alarms)", "Foils (false alarms)")
    )
  )

ggplot(plot_dat, aes(x = measure, y = rate)) +
  stat_summary(fun = mean, geom = "col", width = 0.65) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(
    title = "Recognition performance in the DRM task",
    x = NULL,
    y = "Proportion 'Old' responses"
  ) +
  theme_minimal(base_size = 14)

# Test the key false-memory effect

#The classic classroom comparison is:
  # false alarms to critical lures
  # vs.
  # false alarms to unrelated foils
t.test(dat$lure_fa_rate, dat$foil_fa_rate, paired = TRUE)

#If the DRM effect worked, lure false alarms should be significantly higher than foil false alarms.

#Optional confidence analysis
conf_dat <- dat %>%
  select(studied_conf, lure_conf, foil_conf) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "confidence") %>%
  mutate(
    measure = factor(
      measure,
      levels = c("studied_conf", "lure_conf", "foil_conf"),
      labels = c("Studied words", "Critical lures", "Foils")
    )
  )

ggplot(conf_dat, aes(x = measure, y = confidence)) +
  stat_summary(fun = mean, geom = "col", width = 0.65) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(
    title = "Confidence by item type",
    x = NULL,
    y = "Mean confidence"
  ) +
  theme_minimal(base_size = 14)

# Nice long-format table for class discussion
class_summary <- dat %>%
  summarise(
    Hits = mean(hit_rate, na.rm = TRUE),
    LureFalseAlarms = mean(lure_fa_rate, na.rm = TRUE),
    FoilFalseAlarms = mean(foil_fa_rate, na.rm = TRUE),
    StudiedConfidence = mean(studied_conf, na.rm = TRUE),
    LureConfidence = mean(lure_conf, na.rm = TRUE),
    FoilConfidence = mean(foil_conf, na.rm = TRUE)
  )

class_summary