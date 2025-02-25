if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

# 1)How many hospitals filed more than one report in the same year? Show your answer as a line graph of the number of hospitals over time. 
multi_report_counts <- duplicate.hcris %>%
group_by(fyear) %>%
summarise(num_hospitals = n_distinct(provider_number))

## Plot the number of hospitals filing multiple reports by year
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


# 2) After removing/combining multiple reports, how many unique hospital IDs (Medicare provider numbers) exist in the data?
unique_hospital_count <- final.hcris.data %>%
  distinct(provider_number) %>%
  nrow()
## Print the result
cat("Number of unique hospital IDs (Medicare provider numbers):", unique_hospital_count, "\n")

# 3)What is the distribution of total charges in each year? 
ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
scale_y_log10() + # Apply log scale to handle skewed distributions
labs(
title = "Distribution of Total Charges by Year",
x = "Year",
y = "Total Charges (log scale)",
caption = "Source: HCRIS Data (1996 & 2010 Versions)"
) +
theme_minimal()


# 4)What is the distribution of estimated prices in each year?
## Step 1: Calculate discount factor
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = if_else(price_denom > 0, price_num / price_denom, NA_real_) # Avoid division by zero or negative denominator
  )

## Step 2: Filter out negative or invalid prices and any other outliers (e.g., top 1% of prices)
final.hcris.data <- final.hcris.data %>%
  filter(!is.na(price) & price > 0)

## Step 3: Create a violin plot to show the distribution of estimated prices by year
final.hcris.data <- final.hcris.data %>%
  mutate(log_price = log(price))

ggplot(final.hcris.data, aes(x = as.factor(year), y = log_price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 5)Calculate the average price among penalized versus non-penalized hospitals. 
final.hcris.data <- final.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

final.hcris.2012 <- final.hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

mean.pen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==1)]),2)
mean.nopen <- round(mean(final.hcris.2012$price[which(final.hcris.2012$penalty==0)]),2)
print(mean.pen)
print(mean.nopen)


# 6) Split hospitals into quartiles based on bed size. Provide a table of the average price among treated/control groups for each quartile. 
## Define penalty: HVBP + HRRP < 0
final.hcris.2012 <- final.hcris.2012 %>%
mutate(
hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
hrrp_payment = ifelse(is.na(hrrp_payment), 0, hrrp_payment),
penalty = (hvbp_payment + hrrp_payment) < 0
)

## Calculate bed size quartiles
bed_quartiles <- quantile(final.hcris.2012$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## Assign each hospital to a bed size quartile
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(
    Q1 = ifelse(beds <= bed_quartiles[1], 1, 0),
    Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
    Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
    Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
  )

## Calculate average prices by quartile and penalty status
quartile_summary <- final.hcris.2012 %>%
mutate(bed_quartile = case_when(
Q1 == 1 ~ "Q1",
Q2 == 1 ~ "Q2",
Q3 == 1 ~ "Q3",
Q4 == 1 ~ "Q4"
)) %>%
group_by(bed_quartile, penalty) %>%
summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

print(quartile_summary)

# 7) Find the average treatment effect using each of the following estimators, and present your results in a single table:

# Prepare data properly
matching_data <- final.hcris.2012 %>%
  filter(!is.na(price) & !is.na(penalty) & !is.na(Q1) & !is.na(Q2) & !is.na(Q3) & !is.na(Q4))

# Create variables
lp.vars <- final.hcris.2012 %>% dplyr::select(Q1, Q2, Q3, Q4, penalty, price) %>% filter(complete.cases(.))

lp.covs <- lp.vars %>% dplyr::select(penalty,price)

# Diagnostic checks
cat("Rows in lp.vars:", nrow(lp.vars), "\n")
cat("Rows in lp.covs:", nrow(lp.covs), "\n")
print(cor(lp.covs))

# Nearest Neighbor Matching (Inverse Variance)
m.nn.var <- Matching::Match(
  Y = lp.vars$price,
  Tr = lp.vars$penalty,
  X = lp.covs,
  M = 1,
  Weight = 1,
  estimand = "ATE"
)
summary(m.nn.var)

# 2️⃣ Nearest Neighbor Matching (Mahalanobis)
m.nn.md <- Matching::Match(
  Y = lp.vars$price,
  Tr = lp.vars$penalty,
  X = lp.covs,
  M = 1,
  Weight = 2,
  estimand = "ATE"
)
summary(m.nn.md)

# Inverse Propensity Weighting
logit.model <- glm(penalty ~ Q1 + Q2 + Q3 + Q4, family=binomial, data=lp.vars)
ps <- fitted(logit.model)

m.nn.ps <- Matching::Match(
  Y = lp.vars$price,
  Tr = lp.vars$penalty,
  X = ps,
  M = 1,
  estimand = "ATE"
)
summary(m.nn.ps)

# Simple Linear Regression
linreg.model <- lm(price ~ penalty * (Q1 + Q2 + Q3), data = lp.vars)
summary(linreg.model)

# Extract ATE and SE
linreg.ate <- coef(linreg.model)[grep("penalty", names(coef(linreg.model)))[1]]
linreg.se <- coef(summary(linreg.model))[grep("penalty", rownames(coef(summary(linreg.model))))[1], "Std. Error"]

# Collect results
nn_invvar_ate <- summary(m.nn.var)$est
nn_invvar_se <- summary(m.nn.var)$se

nn_md_ate <- summary(m.nn.md)$est
nn_md_se <- summary(m.nn.md)$se

ipw_ate <- summary(m.nn.ps)$est
ipw_se <- summary(m.nn.ps)$se

# Final summary table
results_table <- tibble(
  Estimator = c("NN Matching (Inverse Variance)", 
                "NN Matching (Mahalanobis)", 
                "Inverse Propensity Weighting", 
                "Simple Linear Regression"),
  ATE = c(nn_invvar_ate, nn_md_ate, ipw_ate, linreg.ate),
  SE = c(nn_invvar_se, nn_md_se, ipw_se, linreg.se)
)

# Print the results
print(results_table)
