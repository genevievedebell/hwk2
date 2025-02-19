m.nn.var <- Matching::Match(
  Y = final.hcris.2012$price,
  Tr = final.hcris.2012$penalty,
  X = final.hcris.2012 %>% select(Q1, Q2, Q3, Q4) %>% as.matrix(),
  M = 1, 
  Weight = 1, # Inverse variance weighting
  estimand = "ATT" # Use ATT instead of ATE
)
summary(m.nn.var)

m.nn.md <- Matching::Match(
  Y = final.hcris.2012$price,
  Tr = final.hcris.2012$penalty,
  X = final.hcris.2012 %>% select(Q1, Q2, Q3, Q4) %>% as.matrix(),
  M = 1,
  Weight = 2, # Mahalanobis distance
  estimand = "ATT"
)
summary(m.nn.md)

logit.model <- glm(penalty ~ Q1 + Q2 + Q3 + Q4 + beds + tot_charges, 
                   family=binomial, data=final.hcris.2012)

ps <- fitted(logit.model)

# Stabilize weights
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(ipw = case_when(
    penalty == 1 ~ min(10, 1 / ps),    # Cap max weight at 10
    penalty == 0 ~ min(10, 1 / (1 - ps)),   
    TRUE ~ NA_real_
  ))

# Compute weighted means
mean.t1 <- final.hcris.2012 %>% filter(penalty == 1) %>%
  summarize(mean_p = weighted.mean(price, w = ipw, na.rm = TRUE))

mean.t0 <- final.hcris.2012 %>% filter(penalty == 0) %>%
  summarize(mean_p = weighted.mean(price, w = ipw, na.rm = TRUE))

ipw.ate <- mean.t1$mean_p - mean.t0$mean_p
print(ipw.ate)

reg <- lm(price ~ penalty + Q1 + Q2 + Q3 + Q4, data = final.hcris.2012)
summary(reg)

linreg.ate <- coef(reg)["penalty"]
print(linreg.ate)
results_table <- tibble(
  Estimator = c("NN Matching (Inverse Variance)", 
                "NN Matching (Mahalanobis)", 
                "Inverse Propensity Weighting", 
                "Simple Linear Regression"),
  ATE = c(summary(m.nn.var)$est, summary(m.nn.md)$est, ipw.ate, linreg.ate),
  SE = c(summary(m.nn.var)$se, summary(m.nn.md)$se, NA, NA)
)
print(results_table)

final.hcris.2012 %>%
  group_by(penalty) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    count = n()
  )
final.hcris.2012 %>%
  summarise(
    Q1_min = min(beds[Q1 == 1]),
    Q1_max = max(beds[Q1 == 1]),
    Q2_min = min(beds[Q2 == 1]),
    Q2_max = max(beds[Q2 == 1]),
    Q3_min = min(beds[Q3 == 1]),
    Q3_max = max(beds[Q3 == 1]),
    Q4_min = min(beds[Q4 == 1]),
    Q4_max = max(beds[Q4 == 1])
  )
