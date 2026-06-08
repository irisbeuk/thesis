# Thesis Analysis Script
# Author: Iris Beukering
# Date: 2026‑03‑06
# Purpose: Import and clean Qualtrics data

# Load packages
library(psych)
library(dplyr)


# Import data
data <- read.csv("data_qualtrics_value.csv")

# Remove the first two metadata rows
data <- data[-c(1, 2), ]

# Rename variables for easier use
data <- data %>% rename(
  bt1 = Brand.trust_1,
  bt2 = Brand.trust_2,
  bt3 = Brand.trust_3,
  bt4 = Brand.trust_4,
  
  sc1 = scarcity.credibility_1,
  sc2 = scarcity.credibility_2,
  sc3 = scarcity.credibility_3,
  sc4 = scarcity.credibility_4,
  sc5 = scarcity.credibility_5, # attention check
  
  sp1 = Socialproof.credibil_1,
  sp2 = Socialproof.credibil_2,
  sp3 = Socialproof.credibil_3,
  sp4 = Socialproof.credibil_4,
  sp5 = Socialproof.credibil_5, # attention check
  
  pi1 = Purchase.intention_1,
  pi2 = Purchase.intention_2,
  pi3 = Purchase.intention_3,
  pi4 = Purchase.intention_4,
  pi5 = Purchase.intention_5,
  
  familiarity = Product.familiarity
)

# Convert only numeric columns (scale items + demographics)
data <- data %>%
  mutate(across(
    c(bt1:pi5, Age, Gender, familiarity),
    ~ suppressWarnings(as.numeric(.))
  ))

# Inspect structure
str(data)
names(data)

table(data$cue_type)
table(data$condition)

#remove failed attention checks
data <- data %>%
  filter(
    (cue_type == "scarcity" & !is.na(sc5) & sc5 == 7) |
      (cue_type == "social_proof" & !is.na(sp5) & sp5 == 7)
  )
dim(data)
table(data$cue_type)
table(data$condition)

# Remove incomplete responses
data <- data %>%
  filter(Finished == 1) %>%
  filter(!is.na(bt1))
summary(data)
table(data$condition)

# Compute Scale Means: credibility
data$credibility_mean <- ifelse(
  data$cue_type == "scarcity",
  rowMeans(data[, c("sc1","sc2","sc3","sc4")], na.rm = TRUE),
  rowMeans(data[, c("sp1","sp2","sp3","sp4")], na.rm = TRUE)
)
head(data[, c("sc1","sc2","sc3","sc4","sp1","sp2","sp3","sp4","credibility_mean")])

# Brand trust mean
data$brandtrust_mean <- rowMeans(data[, c("bt1","bt2","bt3","bt4")], na.rm = TRUE)

# Purchase intention mean
data$purchaseintent_mean <- rowMeans(data[, c("pi1","pi2","pi3","pi4","pi5")], na.rm = TRUE)

# Reliability & validity: cronbahc's alpha
alpha(data[, c("bt1","bt2","bt3","bt4")])$total$raw_alpha
alpha(data[, c("sc1","sc2","sc3","sc4")])$total$raw_alpha
alpha(data[, c("sp1","sp2","sp3","sp4")])$total$raw_alpha
alpha(data[, c("pi1","pi2","pi3","pi4","pi5")])$total$raw_alpha

# Brand trust Principal Axis Factoring (PAF):
fa(data[, c("bt1","bt2","bt3","bt4")], nfactors = 1, fm = "pa")

# sc Principal Axis Factoring (PAF):
fa(data[, c("sc1","sc2","sc3","sc4")], nfactors = 1, fm = "pa")

# sp Principal Axis Factoring (PAF):
fa(data[, c("sp1","sp2","sp3","sp4")], nfactors = 1, fm = "pa")

# pi Principal Axis Factoring (PAF):
fa(data[, c("pi1","pi2","pi3","pi4","pi5")], nfactors = 1, fm = "pa")

# mean & SD for factor analysis
mean(data$bt1, na.rm = TRUE)
sd(data$bt1, na.rm = TRUE)
mean(data$bt2, na.rm = TRUE)
sd(data$bt2, na.rm = TRUE)
mean(data$bt3, na.rm = TRUE)
sd(data$bt3, na.rm = TRUE)
mean(data$bt4, na.rm = TRUE)
sd(data$bt4, na.rm = TRUE)


mean(data$sc1, na.rm = TRUE)
sd(data$sc1, na.rm = TRUE)
mean(data$sc2, na.rm = TRUE)
sd(data$sc2, na.rm = TRUE)
mean(data$sc3, na.rm = TRUE)
sd(data$sc3, na.rm = TRUE)
mean(data$sc4, na.rm = TRUE)
sd(data$sc4, na.rm = TRUE)


mean(data$sp1, na.rm = TRUE)
sd(data$sp1, na.rm = TRUE)
mean(data$sp2, na.rm = TRUE)
sd(data$sp2, na.rm = TRUE)
mean(data$sp3, na.rm = TRUE)
sd(data$sp3, na.rm = TRUE)
mean(data$sp4, na.rm = TRUE)
sd(data$sp4, na.rm = TRUE)


mean(data$pi1, na.rm = TRUE)
sd(data$pi1, na.rm = TRUE)
mean(data$pi2, na.rm = TRUE)
sd(data$pi2, na.rm = TRUE)
mean(data$pi3, na.rm = TRUE)
sd(data$pi3, na.rm = TRUE)
mean(data$pi4, na.rm = TRUE)
sd(data$pi4, na.rm = TRUE)
mean(data$pi5, na.rm = TRUE)
sd(data$pi5, na.rm = TRUE)

# Chapter 4.1
install.packages("pwr")
library(pwr)
pwr.anova.test(k = 4, f = 0.25, sig.level = 0.05, power = 0.80)
table(data$condition)

# Sample characteristics
# Age distribution
age_dist <- round(prop.table(table(data$Age)) * 100, 1)
age_n <- table(data$Age)
age_summary <- data.frame(Age = names(age_dist),
                          Percent = age_dist,
                          N = age_n)

age_summary
age_n <- table (data$Age)

# Gender distribution
# Gender distribution
gender_tab <- table(data$Gender)
gender_pct <- round(prop.table(gender_tab) * 100, 1)

Gender_summary <- data.frame(
  Gender = names(gender_tab),
  Percent = gender_pct,
  N = as.numeric(gender_tab)
)

Gender_summary

# familiarity distribution
# Familiarity distribution
fam_tab <- table(data$familiarity)
fam_pct <- round(prop.table(fam_tab) * 100, 1)

familiarity_summary <- data.frame(
  Familiarity = names(fam_tab),
  Percent = fam_pct,
  N = as.numeric(fam_tab)
)

familiarity_summary

# descriptive statistics for composite scales
# brand trust
mean(data$brandtrust_mean, na.rm = TRUE)
sd(data$brandtrust_mean, na.rm = TRUE)
# purchase intention
mean(data$purchaseintent_mean, na.rm = TRUE)
sd(data$purchaseintent_mean, na.rm = TRUE)
# credibility
mean(data$credibility_mean, na.rm = TRUE)
sd(data$credibility_mean, na.rm = TRUE)

#4.2 
#levels of trust variables
data$trust_level <- ifelse(grepl("hightrust", data$condition), "High", "Low")
table(data$trust_level)
# Means and SD per condition
aggregate(brandtrust_mean ~ trust_level, data = data,
          FUN = function(x) c(M = mean(x), SD = sd(x), N = length(x)))
#independent-sample t test
t.test(brandtrust_mean ~ trust_level, data = data)

#bar chart for 4.2.1: NOT INCLUDED IN THESIS
library(ggplot2)
library(dplyr)

# Prepare summary data
trust_summary <- data %>%
  group_by(trust_level) %>%
  summarise(
    M = mean(brandtrust_mean, na.rm = TRUE),
    SD = sd(brandtrust_mean, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N)
  )

# Bar chart : NOT INCLUDED IN THESIS
ggplot(trust_summary, aes(x = trust_level, y = M, fill = trust_level)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.15, size = 0.8) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(
    x = "Brand Trust Condition",
    y = "Mean Brand Trust",
    title = "Brand Trust Manipulation Check"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#4.2.1 credibility check
data$credibility_mean
table(data$cue_type)
aggregate(credibility_mean ~ cue_type, data = data,
          FUN = function(x) c(M = mean(x), SD = sd(x), N = length(x)))
t.test(credibility_mean ~ cue_type, data = data)

#4.3 correlation test
library(psych)

corr.test(data[, c("brandtrust_mean", "credibility_mean", "purchaseintent_mean")])

#4.4.1 2x2 ANOVA for the main effect
anova_model <- aov(purchaseintent_mean ~ trust_level * cue_type, data = data)
summary(anova_model)
aggregate(purchaseintent_mean ~ trust_level * cue_type, data = data,
          FUN = function(x) c(M = mean(x), SD = sd(x), N = length(x)))

#make a plot
library(ggplot2)
library(dplyr)

# Compute means and SEs per condition
plot_data <- data %>%
  group_by(trust_level, cue_type) %>%
  summarise(
    mean_PI = mean(purchaseintent_mean),
    se_PI = sd(purchaseintent_mean) / sqrt(n())
  )

# Create the interaction plot
ggplot(plot_data, aes(x = cue_type, y = mean_PI, 
                      group = trust_level, color = trust_level)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_PI - se_PI, ymax = mean_PI + se_PI),
                width = 0.1, size = 0.8) +
  scale_color_manual(values = c("High" = "#1f78b4", "Low" = "#e31a1c")) +
  labs(
    x = "Cue Type",
    y = "Purchase Intention",
    color = "Brand Trust",
    title = "Interaction Between Brand Trust and Cue Type on Purchase Intention"
  ) +
  theme_minimal(base_size = 14)

# another cleaner look?
library(ggplot2)
library(dplyr)

# Compute means and SEs per condition
plot_data <- data %>%
  group_by(trust_level, cue_type) %>%
  summarise(
    mean_PI = mean(purchaseintent_mean),
    se_PI = sd(purchaseintent_mean) / sqrt(n())
  )

# Create the plot styled like the example
ggplot(plot_data, aes(x = cue_type, y = mean_PI, 
                      group = trust_level, color = trust_level, linetype = trust_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_PI - se_PI, ymax = mean_PI + se_PI),
                width = 0.1, size = 0.8) +
  scale_color_manual(values = c("High" = "#1f78b4", "Low" = "#e31a1c")) +
  scale_linetype_manual(values = c("High" = "dashed", "Low" = "solid")) +
  labs(
    x = "Cue Type",
    y = "Purchase Intention",
    color = "Brand Trust",
    linetype = "Brand Trust"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_blank()
  )

# ONE MORE VERSION: final used ver
library(ggplot2)
library(dplyr)

# Compute means, SDs, Ns, SEs, and 95% CIs
plot_data <- data %>%
  group_by(trust_level, cue_type) %>%
  summarise(
    mean_PI = mean(purchaseintent_mean),
    sd_PI = sd(purchaseintent_mean),
    n = n(),
    se_PI = sd_PI / sqrt(n),
    t_crit = qt(.975, df = n - 1),
    ci_lower = mean_PI - t_crit * se_PI,
    ci_upper = mean_PI + t_crit * se_PI
  )

# Plot with 95% CI error bars
ggplot(plot_data, aes(x = cue_type, y = mean_PI,
                      group = trust_level, color = trust_level, linetype = trust_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.12, size = 0.9) +
  scale_color_manual(values = c("High" = "#1f78b4", "Low" = "#e31a1c")) +
  scale_linetype_manual(values = c("High" = "dashed", "Low" = "solid")) +
  labs(
    x = "Cue Type",
    y = "Purchase Intention",
    color = "Brand Trust",
    linetype = "Brand Trust",
    title = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

str(data[, c("bt1","bt2","bt3","bt4")])

# 4.4.2 mediation 1: BT -> credibility -> PI
library(lavaan)

model_trust_med <- '
  # a-path
  credibility_mean ~ a * brandtrust_mean

  # b- and c’-paths
  purchaseintent_mean ~ cprime * brandtrust_mean + b * credibility_mean

  # indirect and total effects
  indirect := a * b
  total := cprime + (a * b)
'

fit_trust_med <- sem(model_trust_med, data = data, se = "bootstrap", bootstrap = 5000)
summary(fit_trust_med, standardized = TRUE, ci = TRUE)
summary(fit_trust_med, standardized = TRUE, ci = TRUE)

#make it tidy

trust_med_results <- parameterEstimates(fit_trust_med, standardized = TRUE, ci = TRUE)
View(trust_med_results)

# 4.4.2 cue type
library(lavaan)

model_cue_med <- '
  # a-path
  credibility_mean ~ a*cue_type

  # b-path
  purchaseintent_mean ~ b*credibility_mean


purchaseintent_mean ~ cprime*cue_type

# indirect effect
indirect := a*b

# total effect
total := cprime + (a*b)
'
my_data <- data

fit_cue_med <- sem(
  model_cue_med,
  data = my_data,
  se = "bootstrap",
  bootstrap = 5000
)

summary(fit_cue_med, standardized = FALSE, ci = TRUE)

###
library(lavaan)

# 1. Dummy maken
data$cue_dummy <- ifelse(data$cue_type == "social_proof", 1, 0)

# 2. Interactie maken
data$cue_trust_int <- data$cue_dummy * data$brandtrust_mean

# 3. Means en SD buiten lavaan berekenen
m_trust <- mean(data$brandtrust_mean, na.rm = TRUE)
sd_trust <- sd(data$brandtrust_mean, na.rm = TRUE)

# 4. Model 8 specificeren
model8 <- paste0('
  credibility_mean ~ a1*cue_dummy + a2*brandtrust_mean + a3*cue_trust_int
  purchaseintent_mean ~ b*credibility_mean + cprime*cue_dummy + d*brandtrust_mean

  ind_low := (a1 + a3*(', m_trust - sd_trust ,')) * b
  ind_mean := (a1 + a3*(', m_trust ,')) * b
  ind_high := (a1 + a3*(', m_trust + sd_trust ,')) * b

  imm := a3 * b
')

fit_model8 <- sem(
  model8,
  data = data,
  se = "bootstrap",
  bootstrap = 5000
)

summary(fit_model8, standardized = FALSE, ci = TRUE)

#4.4.3
# Additional analyses with covariates
model8_cov <- '
  credibility_mean ~ a1*cue_dummy + a2*brandtrust_mean + a3*cue_trust_int
  purchaseintent_mean ~ b*credibility_mean + cprime*cue_dummy + d*brandtrust_mean +
                        Age + Gender + familiarity
'

fit_model8_cov <- sem(
  model8_cov,
  data = data,
  se = "bootstrap",
  bootstrap = 5000
)

summary(fit_model8_cov, standardized = FALSE, ci = TRUE)


