library(dplyr)
library(ggplot2)

# Count hospitals that filed multiple reports in the same year (1996 version)
duplicates_1996 <- final.hcris.v1996 %>%
  group_by(year, provider_number) %>%
  summarise(report_count = n(), .groups = "drop") %>%
  filter(report_count > 1) %>%
  group_by(year) %>%
  summarise(num_hospitals = n(), .groups = "drop") %>%
  mutate(version = "1996 Form")

# Count hospitals that filed multiple reports in the same year (2010 version)
duplicates_2010 <- final.hcris.v2010 %>%
  group_by(year, provider_number) %>%
  summarise(report_count = n(), .groups = "drop") %>%
  filter(report_count > 1) %>%
  group_by(year) %>%
  summarise(num_hospitals = n(), .groups = "drop") %>%
  mutate(version = "2010 Form")

# Combine both datasets for plotting
duplicate_hospitals <- bind_rows(duplicates_1996, duplicates_2010)

# Plot the line graph with both versions
ggplot(duplicate_hospitals, aes(x = year, y = num_hospitals, color = version, group = version)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Hospitals Filing Multiple Reports in the Same Year",
       x = "Year",
       y = "Number of Hospitals",
       color = "HCRIS Version",
       caption = "Source: HCRIS Data (1996 & 2010 Versions)") +
  theme_minimal()

# Remove duplicate reports within the same year for each dataset
dedup_1996 <- final.hcris.v1996 %>%
  group_by(year, provider_number) %>%
  summarise(.groups = "drop")  # Keep one entry per hospital per year
