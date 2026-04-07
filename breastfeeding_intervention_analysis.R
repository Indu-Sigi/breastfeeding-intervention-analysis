# Breastfeeding Intervention Study Analysis
# Student course project (graduate level)
# Author: Indu Sigicharla
# Note: Dataset not included

# ---- load packages ----
library(readxl)
library(dplyr)
library(tableone)
library(gtsummary)
library(survival)
library(survminer)
library(sandwich)
library(lmtest)
library(broom)
library(epitools)

# ---- read dataset ----
df <- read_excel("BFStudy.xlsx")

# ---- set variable types ----
df <- df %>%
  mutate(id = as.character(id),
         InHospInt = as.integer(InHospInt),
         AtHomeInt = ifelse(is.na(AtHomeInt), NA, as.integer(AtHomeInt)),
         educ = factor(educ,
                       levels = c(1, 2, 3),
                       labels = c("HS_or_less", "Some_college", "College_deg")),
         employ = factor(employ, levels = c(0, 1), labels = c("Not_employed", "Employed")),
         mage = as.numeric(mage),
         BFinit = as.integer(BFinit),
         BFweeks = as.numeric(BFweeks),
         BFstopby6mo = as.integer(BFstopby6mo))

# PART 1: In-Hospital Intervention and Breastfeeding Initiation----
# ---- descriptive table by In-Hospital intervention arm ----
vars_part1 <- c("mage", "educ", "employ")

cat("Descriptive table for In-Hospital Intervention vs Control\n")

table1_inhosp <- CreateTableOne(vars = vars_part1,
                                strata = "InHospInt",
                                data = df,
                                factorVars = c("educ", "employ"))

print(table1_inhosp, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# ---- crude 2x2 summary ----
bfinit_by_inhosp <- df %>%
  group_by(InHospInt) %>%
  summarise(n = n(),
            n_init = sum(BFinit == 1, na.rm = TRUE),
            prop_init = n_init / n,
            .groups = "drop")

bfinit_by_inhosp

# 2x2 table: rows = intervention group, cols = BF initiation
tab_inhosp_bfinit <- table(df$InHospInt, df$BFinit)
tab_inhosp_bfinit

# Chi-square test
chisq.test(tab_inhosp_bfinit)

# Optional: crude RR
rr_crude <- riskratio(tab_inhosp_bfinit)
rr_crude

# ---- main crude analysis for Aim 1: logistic regression ----
df_part1 <- df %>%
  filter(!is.na(BFinit))

logit_crude <- glm(BFinit ~ InHospInt, family = binomial, data = df_part1)
summary(logit_crude)

part1_crude_or <- tidy(logit_crude, exponentiate = TRUE, conf.int = TRUE)
part1_crude_or

# ---- adjusted logistic regression for Aim 1 ----
logit_adj <- glm(BFinit ~ InHospInt + mage + educ + employ,family = binomial,data = df_part1)

summary(logit_adj)

part1_adjusted_or <- tidy(logit_adj, exponentiate = TRUE, conf.int = TRUE)
part1_adjusted_or

# ---- optional adjusted RR approach using modified Poisson ----
mod_poisson <- glm(BFinit ~ InHospInt + mage + educ + employ, family = poisson(link = "log"),
                   data = df_part1)

cov_poisson_robust <- vcovHC(mod_poisson, type = "HC0")
se_poisson_robust <- sqrt(diag(cov_poisson_robust))

poisson_coefs <- coeftest(mod_poisson, vcov. = cov_poisson_robust)
poisson_coefs

beta <- coef(mod_poisson)
z_crit <- qnorm(0.975)

part1_adjusted_rr <- data.frame(term = names(beta),
                                estimate = exp(beta),
                                lower = exp(beta - z_crit * se_poisson_robust),
                                upper = exp(beta + z_crit * se_poisson_robust),
                                se = se_poisson_robust,
                                z = beta / se_poisson_robust,
                                p = 2 * pnorm(-abs(beta / se_poisson_robust)))

part1_adjusted_rr

# PART 2: At-Home Intervention and Time to Breastfeeding Cessation----

# Restrict to those who initiated breastfeeding
df_phase2 <- df %>%
  filter(BFinit == 1) %>%
  mutate(AtHomeInt_f = factor(AtHomeInt,
                              levels = c(0, 1),
                              labels = c("Control", "AtHomeIntervention")),
         InHospInt_f = factor(InHospInt,
                              levels = c(0, 1),
                              labels = c("InHospControl", "InHospIntervention")))

nrow(df_phase2)

# ---- descriptive table by At-Home intervention arm ----
vars_part2 <- c("mage", "educ", "employ", "InHospInt_f")

cat("Descriptive table for At-Home Intervention vs Control\n")

table2_athome <- CreateTableOne(vars = vars_part2,
                                strata = "AtHomeInt_f",
                                data = df_phase2,
                                factorVars = c("educ", "employ", "InHospInt_f"))

print(table2_athome, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# ---- survival object ----
surv_obj <- with(df_phase2, Surv(time = BFweeks, event = BFstopby6mo == 1))

summary(df_phase2$BFweeks)

# ---- unadjusted Kaplan-Meier curves ----
fit_km <- survfit(surv_obj ~ AtHomeInt, data = df_phase2)

km_plot <- ggsurvplot(fit_km,
                      data = df_phase2,
                      risk.table = TRUE,
                      xlab = "Weeks since discharge",
                      ylab = "Probability still feeding breast milk",
                      legend.labs = c("Control", "AtHomeIntervention"))

print(km_plot)

# ---- log-rank test ----
survdiff(surv_obj ~ AtHomeInt, data = df_phase2)

# ---- unadjusted Cox model ----
cox_unadj <- coxph(surv_obj ~ AtHomeInt, data = df_phase2)
summary(cox_unadj)

part2_crude_hr <- tidy(cox_unadj, exponentiate = TRUE, conf.int = TRUE)
part2_crude_hr

# ---- adjusted Cox model ----
cox_adj <- coxph(surv_obj ~ AtHomeInt + InHospInt + mage + educ + employ,
                 data = df_phase2)

summary(cox_adj)

part2_adjusted_hr <- tidy(cox_adj, exponentiate = TRUE, conf.int = TRUE)
part2_adjusted_hr

# PART 3: Effect Modification by In-Hospital Intervention----

# ---- Cox model with interaction ----
cox_interaction <- coxph(surv_obj ~ AtHomeInt * InHospInt + mage + educ + employ,
                         data = df_phase2)

summary(cox_interaction)

# Likelihood ratio test comparing models with and without interaction
anova(cox_adj, cox_interaction, test = "LRT")

part3_interaction_hr <- tidy(cox_interaction, exponentiate = TRUE, conf.int = TRUE)
part3_interaction_hr

# ---- stratified analyses: At-Home effect within In-Hospital groups ----

# Among those who received In-Hospital intervention
df_inhosp1 <- df_phase2 %>%
  filter(InHospInt == 1)

cox_inhosp1 <- coxph(Surv(BFweeks, BFstopby6mo == 1) ~ AtHomeInt + mage + educ + employ,
                     data = df_inhosp1)

summary(cox_inhosp1)

part3_strat_hr_inhosp1 <- tidy(cox_inhosp1, exponentiate = TRUE, conf.int = TRUE)
part3_strat_hr_inhosp1

# Among those who did NOT receive In-Hospital intervention
df_inhosp0 <- df_phase2 %>%
  filter(InHospInt == 0)

cox_inhosp0 <- coxph(Surv(BFweeks, BFstopby6mo == 1) ~ AtHomeInt + mage + educ + employ,
                     data = df_inhosp0)

summary(cox_inhosp0)

part3_strat_hr_inhosp0 <- tidy(cox_inhosp0, exponentiate = TRUE, conf.int = TRUE)
part3_strat_hr_inhosp0