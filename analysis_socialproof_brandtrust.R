# ============================================================
# Analysis: Is Social Proof Credibility dependent on Brand Trust?
# ============================================================
# This script tests whether the credibility of social proof
# (i.e., how believable/trustworthy the "Top Rated" message is)
# depends on the consumer's brand trust.
# ============================================================

library(psych)
library(dplyr)

# ---- 1. Data Import & Cleaning (same as thesis.R) ----
data <- read.csv("data_qualtrics_value.csv")
data <- data[-c(1, 2), ]

data <- data |> rename(
  bt1 = Brand.trust_1, bt2 = Brand.trust_2, bt3 = Brand.trust_3, bt4 = Brand.trust_4,
  sc1 = scarcity.credibility_1, sc2 = scarcity.credibility_2,
  sc3 = scarcity.credibility_3, sc4 = scarcity.credibility_4, sc5 = scarcity.credibility_5,
  sp1 = Socialproof.credibil_1, sp2 = Socialproof.credibil_2,
  sp3 = Socialproof.credibil_3, sp4 = Socialproof.credibil_4, sp5 = Socialproof.credibil_5,
  pi1 = Purchase.intention_1, pi2 = Purchase.intention_2,
  pi3 = Purchase.intention_3, pi4 = Purchase.intention_4, pi5 = Purchase.intention_5,
  familiarity = Product.familiarity
)

data <- data |>
  mutate(across(c(bt1:pi5, Age, Gender, familiarity), ~ suppressWarnings(as.numeric(.))))

# Attention checks
data <- data |>
  filter(
    (cue_type == "scarcity" & !is.na(sc5) & sc5 == 7) |
      (cue_type == "social_proof" & !is.na(sp5) & sp5 == 7)
  )

# Remove incomplete
data <- data |>
  filter(Finished == 1) |>
  filter(!is.na(bt1))

# Compute scale means
data$credibility_mean <- ifelse(
  data$cue_type == "scarcity",
  rowMeans(data[, c("sc1", "sc2", "sc3", "sc4")], na.rm = TRUE),
  rowMeans(data[, c("sp1", "sp2", "sp3", "sp4")], na.rm = TRUE)
)
data$brandtrust_mean <- rowMeans(data[, c("bt1", "bt2", "bt3", "bt4")], na.rm = TRUE)
data$purchaseintent_mean <- rowMeans(data[, c("pi1", "pi2", "pi3", "pi4", "pi5")], na.rm = TRUE)

# ---- 2. Select only Social Proof group ----
sp_data <- data |> filter(cue_type == "social_proof")
cat("========================================\n")
cat("Aantal respondenten in social_proof groep:", nrow(sp_data), "\n")
cat("========================================\n\n")

# ---- 3. Descriptive Statistics ----
cat("=== DESCRIPTIVES: Social Proof Group ===\n")
cat("Brand Trust: M =", round(mean(sp_data$brandtrust_mean, na.rm = TRUE), 2),
    ", SD =", round(sd(sp_data$brandtrust_mean, na.rm = TRUE), 2), "\n")
cat("Social Proof Credibility: M =", round(mean(sp_data$credibility_mean, na.rm = TRUE), 2),
    ", SD =", round(sd(sp_data$credibility_mean, na.rm = TRUE), 2), "\n\n")

# ---- 4. Pearson Correlatie ----
cat("=== CORRELATIE: Brand Trust vs Social Proof Credibility ===\n")
cor_test <- cor.test(sp_data$brandtrust_mean, sp_data$credibility_mean, use = "complete.obs")
cat("Pearson r =", round(cor_test$estimate, 3), "\n")
cat("95% CI: [", round(cor_test$conf.int[1], 3), ", ", round(cor_test$conf.int[2], 3), "]\n")
cat("t(", cor_test$parameter, ") =", round(cor_test$statistic, 2), "\n")
cat("p =", format(cor_test$p.value, scientific = FALSE), "\n\n")

# ---- 5. Lineaire Regressie ----
cat("=== REGRESSIE: Credibility ~ Brand Trust ===\n")
model <- lm(credibility_mean ~ brandtrust_mean, data = sp_data)
summary_model <- summary(model)
print(summary_model)

# Extract key values
beta <- round(coef(model)[2], 3)
p_val <- round(summary_model$coefficients[2, 4], 4)
r2 <- round(summary_model$r.squared, 3)
cat("\n--> Interpretatie:\n")
cat("    Beta (ongestandaardiseerd) =", beta, "\n")
cat("    R-squared =", r2, "\n")
cat("    p =", p_val, "\n")
if (p_val < 0.05) {
  cat("    CONCLUSIE: Brand trust heeft een significant effect op social proof credibility.\n")
} else {
  cat("    CONCLUSIE: Brand trust heeft GEEN significant effect op social proof credibility.\n")
}

# ---- 6. Vergelijking per conditie (low vs high trust) ----
cat("\n=== GEMIDDELDEN PER CONDITIE ===\n")
sp_data |>
  group_by(condition) |>
  summarise(
    n = n(),
    mean_brandtrust = round(mean(brandtrust_mean, na.rm = TRUE), 2),
    sd_brandtrust = round(sd(brandtrust_mean, na.rm = TRUE), 2),
    mean_sp_credibility = round(mean(credibility_mean, na.rm = TRUE), 2),
    sd_sp_credibility = round(sd(credibility_mean, na.rm = TRUE), 2)
  ) |>
  print()

# ---- 7. Independent Samples T-test ----
cat("\n=== T-TEST: Credibility ~ Trust Condition ===\n")
cat("Vergelijkt lowtrust_socialproof vs hightrust_socialproof\n\n")
t_test <- t.test(credibility_mean ~ condition,
  data = sp_data,
  subset = condition %in% c("lowtrust_socialproof", "hightrust_socialproof")
)
print(t_test)

# ---- 8. Visualisatie (optioneel) ----
# Scatterplot met regressielijn
plot(sp_data$brandtrust_mean, sp_data$credibility_mean,
  main = "Social Proof Credibility vs Brand Trust",
  xlab = "Brand Trust (gemiddelde)",
  ylab = "Social Proof Credibility (gemiddelde)",
  col = ifelse(sp_data$condition == "hightrust_socialproof", "blue", "red"),
  pch = 16
)
abline(model, col = "darkgreen", lwd = 2)
legend("topleft",
  legend = c("High Trust", "Low Trust"),
  col = c("blue", "red"), pch = 16
)

