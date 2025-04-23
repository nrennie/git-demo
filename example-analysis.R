# Load packages -----------------------------------------------------------

library(gtsummary)
library(ggcorrplot)
library(tidyverse)
library(forcats)


# Load LOS ---------------------------------------------------------------

LOS <- read_csv("data/LOS.csv") # nolint

LOS <- LOS |> # nolint
  mutate(
    Organisation = factor(Organisation),
    Organisation = fct_relevel(Organisation, "Trust10", after = Inf)
  )


# Exploratory plots -------------------------------------------------------

ggplot(LOS) +
  geom_histogram(aes(Age))

ggplot(LOS) +
  geom_histogram(aes(LOS))

ggplot(LOS) +
  geom_histogram(aes(Age)) +
  facet_wrap(~Organisation)


# Summary statistics ------------------------------------------------------

mean(LOS$Age)
sd(LOS$Age)
mean(LOS$LOS)
mean(LOS$Death)
table(LOS$organisation)


# Statistical tests -------------------------------------------------------

age1 <- LOS |>
  filter(Age <= 50)
age2 <- LOS |>
  filter(Age > 50)
t.test(age1$LOS, age2$LOS, var.equal = TRUE)
t.test(age1$LOS, age2$LOS, var.equal = TRUE)$p.value


# Modelling ---------------------------------------------------------------

mod1 <- glm(Death ~ Age, data = LOS, family = "binomial")
summary(mod1)

mod2 <- glm(Death ~ Age + LOS, data = LOS, family = "binomial")
summary(mod2)

mod3 <- glm(Death ~ Age + LOS + Organisation, data = LOS, family = "binomial")
summary(mod3)

mod4 <- glm(Death ~ LOS, data = LOS, family = "binomial")
summary(mod4)

mod5 <- glm(Death ~ LOS + Age, data = LOS, family = "binomial")
summary(mod5)

# Results -----------------------------------------------------------------

model_table <- tbl_regression(mod3)

model_table |>
  as_gt() |>
  gtsave("regression_table.docx")
