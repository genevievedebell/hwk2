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

dedup_2010 <- final.hcris.v2010 %>%
  group_by(year, provider_number) %>%
  summarise(.groups = "drop")

# Combine both datasets after deduplication
unique_hospitals <- bind_rows(dedup_1996, dedup_2010) %>%
  distinct(provider_number)  # Count unique hospital IDs across all years

# Get total number of unique hospitals
num_unique_hospitals <- nrow(unique_hospitals)
print(paste("Total number of unique hospitals:", num_unique_hospitals))

# Filter and clean data to ensure tot_charges is numeric
clean_data <- final.hcris.v1996 %>%
  filter(!is.na(tot_charges)) %>%
  mutate(tot_charges = as.numeric(tot_charges))  # Ensure numeric type

# Create a violin plot
ggplot(clean_data, aes(x = factor(year), y = tot_charges)) +
  geom_violin(fill = "skyblue", color = "darkblue", alpha = 0.6) +
  labs(title = "Distribution of Total Charges by Year",
       x = "Year",
       y = "Total Charges",
       caption = "Source: HCRIS Data (1996 Version)") +
  theme_minimal()

# Calculate estimated prices and clean the data
price_data <- final.hcris.v1996 %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  ) %>%
  # Remove outliers and impossible values
  filter(!is.infinite(price) & !is.nan(price) & price > 0 & price < quantile(price, 0.99, na.rm = TRUE)) %>%
  # Remove missing values
  filter(!is.na(price))
# Create violin plot of price distribution by year
ggplot(price_data, aes(x = factor(year), y = price)) +
  geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  scale_y_log10() +  # Apply log scale to handle outliers
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price (log scale)",
    caption = "Source: HCRIS Data (1996 Version) - Prices estimated using provided formula"
  ) +
  theme_minimal()
