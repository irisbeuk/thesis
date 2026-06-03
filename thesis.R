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
data <- data |> rename(
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
data <- data |>
  mutate(across(
    c(bt1:pi5, Age, Gender, familiarity),
    ~ suppressWarnings(as.numeric(.))
  ))

# Inspect structure
str(data)
names(data)

table(data$cue_type)
table(data$condition)

# remove failed attention checks
data <- data |>
  filter(
    (cue_type == "scarcity" & !is.na(sc5) & sc5 == 7) |
      (cue_type == "social_proof" & !is.na(sp5) & sp5 == 7)
  )
dim(data)
table(data$cue_type)
table(data$condition)

# Remove incomplete responses
data <- data |>
  filter(Finished == 1) |>
  filter(!is.na(bt1))
summary(data)
table(data$condition)

# Compute Scale Means: credibility
data$credibility_mean <- ifelse(
  data$cue_type == "scarcity",
  rowMeans(data[, c("sc1", "sc2", "sc3", "sc4")], na.rm = TRUE),
  rowMeans(data[, c("sp1", "sp2", "sp3", "sp4")], na.rm = TRUE)
)
head(data[
  ,
  c(
    "sc1", "sc2", "sc3", "sc4", "sp1", "sp2", "sp3", "sp4",
    "credibility_mean"
  )
])

# Brand trust mean
data$brandtrust_mean <- rowMeans(data[
  ,
  c("bt1", "bt2", "bt3", "bt4")
], na.rm = TRUE)

# Purchase intention mean
data$purchaseintent_mean <- rowMeans(
  data[, c("pi1", "pi2", "pi3", "pi4", "pi5")], na.rm = TRUE
)
