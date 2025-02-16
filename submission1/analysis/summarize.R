library(dplyr)
library(ggplot2)

# Count the number of hospitals with multiple reports per year
multi_report_counts <- duplicate.hcris %>%
  group_by(fyear) %>%
  summarise(num_hospitals = n_distinct(provider_number))

# Plot the number of hospitals filing multiple reports by year
ggplot(multi_report_counts, aes(x = fyear, y = num_hospitals)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Hospitals Filing Multiple Reports Per Year",
    x = "Year",
    y = "Number of Hospitals",
    caption = "Source: HCRIS Data (1996 & 2010 Versions)"
  ) +
  theme_minimal()

# Count the number of unique hospital IDs
unique_hospital_count <- final.hcris.data %>%
  distinct(provider_number) %>%
  nrow()

# Print the result
cat("Number of unique hospital IDs (Medicare provider numbers):", unique_hospital_count, "\n")

# Create violin plot of total charges by year
ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
  geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  scale_y_log10() +  # Apply log scale to handle skewed distributions
  labs(
    title = "Distribution of Total Charges by Year",
    x = "Year",
    y = "Total Charges (log scale)",
    caption = "Source: HCRIS Data (1996 & 2010 Versions)"
  ) +
  theme_minimal()

# Step 1: Calculate discount factor
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = if_else(price_denom > 0, price_num / price_denom, NA_real_) # Avoid division by zero or negative denominator
  )

# Step 2: Filter out negative or invalid prices and any other outliers (e.g., top 1% of prices)
final.hcris.data_clean <- final.hcris.data %>%
  filter(!is.na(price) & price > 0)

# Step 3: Create a violin plot to show the distribution of estimated prices by year
ggplot(final.hcris.data_clean, aes(x = as.factor(year), y = price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


