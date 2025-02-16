# Read in the dataset
hcris.data <- read_rds("data/output/HCRIS_Data_v2010.rds")

# Calculate price and define penalty
hcris.data <- hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  )

# Filter for 2012 and define penalty
final.hcris <- hcris.data %>%
  ungroup() %>%
  filter(
    price_denom > 100, !is.na(price_denom),
    price_num > 0, !is.na(price_num),
    price < 100000,
    beds > 30,
    year == 2012
  ) %>%
  mutate(
    hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
    hrrp_payment = ifelse(is.na(hrrp_payment), 0, abs(hrrp_payment)),
    penalty = (hvbp_payment - hrrp_payment) < 0  # TRUE/FALSE
  )
# Calculate mean prices for penalized vs non-penalized hospitals
mean.pen <- round(mean(final.hcris$price[final.hcris$penalty == TRUE], na.rm = TRUE), 2)
mean.nopen <- round(mean(final.hcris$price[final.hcris$penalty == FALSE], na.rm = TRUE), 2)

# Print results
cat("Mean price for penalized hospitals:", mean.pen, "\n")
cat("Mean price for non-penalized hospitals:", mean.nopen, "\n")

# Calculate quartiles of bed size
bed_quartiles <- quantile(final.hcris$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Add quartile indicators
final.hcris <- final.hcris %>%
  mutate(
    Q1 = ifelse(beds <= bed_quartiles[1], 1, 0),
    Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
    Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
    Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
  )
# Calculate average price by penalty and quartile
quartile_summary <- final.hcris %>%
  mutate(quartile = case_when(
    Q1 == 1 ~ "Q1",
    Q2 == 1 ~ "Q2",
    Q3 == 1 ~ "Q3",
    Q4 == 1 ~ "Q4"
  )) %>%
  group_by(quartile, penalty) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  arrange(quartile, penalty)

# Display the table
quartile_summary_table <- quartile_summary %>%
  tidyr::pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

print(quartile_summary_table)

# Install required packages
 install.packages("MatchIt")    # For nearest neighbor matching
install.packages("WeightIt")    # For inverse propensity weighting
install.packages("broom")       # For cleaning model outputs
install.packages("sandwich")    # For robust standard errors
install.packages("lmtest")

library(dplyr)
library(MatchIt)
library(WeightIt)
library(broom)
library(sandwich)
library(lmtest)
# ----------------------------------------
# 1. Nearest Neighbor Matching (Inverse Variance Distance)
# ----------------------------------------
match.invvar <- matchit(penalty ~ Q1 + Q2 + Q3 + Q4, data = final.hcris,
                        method = "nearest", distance = "inverse.var")
# Extract matched dataset and calculate ATE
matched_data_invvar <- match.data(match.invvar)
ate_invvar <- with(matched_data_invvar, mean(price[penalty == 1]) - mean(price[penalty == 0]))


