#######################################################################################################
# EDUC 643 Assignment 4 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 2/16/23
### Last update: 3/6/23
### Inputs: nerds.csv
### Purpose: load in data, summarize categorical data, fit regresion model w/ categorical predictors and produce results
#######################################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)
library(margins)

# Identify directory location of file
i_am("assignments/keys/assignment_4_key.R")


####################################
#  1. Descriptive statistics
####################################

## Read in the data 
nerds <- read.csv(here("data/nerds.csv"))
str(nerds)

# Recoding level and locale to factor variables
nerds$level <- as.factor(nerds$level)
nerds$locale <- as.factor(nerds$locale)

## 1.1 Focus on categorical variables and construct summary statistics
nerds2 <- select(nerds, c(level, locale)) %>%
                  rename(`Grade-band Level` = level,
                         `Geographic locale` = locale) # Changing variable names to descriptive titles for table

datasummary_skim(nerds2, 
                 type="categorical",
                 histogram=F,
                 output = "assignments/keys/assignment_4_descriptives.docx")

## 1.2 Create bar chart descriptive comparisons

# PPE by level
ggplot(nerds, aes(x=level, y=ppe, color=level, fill=level, alpha=0.2)) +
        geom_violin() +
        stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") +
        ylab("Per-pupil expenditure ($)") + xlab("") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")

# Generate some descriptive statistics by group
nerds %>% group_by(level) %>% summarise(mean(ppe))
nerds %>% group_by(level) %>% summarise(sd(ppe))

ggsave("assignments/keys/assignment_4_descr_level.png")

# Geographic locale
ggplot(nerds, aes(x=locale, y=ppe, color=locale, fill=locale, alpha=0.2)) +
  geom_violin() +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") +
  ylab("Per-pupil expenditure ($)") + xlab("") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Generate some descriptive statistics by group
nerds %>% group_by(locale) %>% summarise(mean(ppe))
nerds %>% group_by(locale) %>% summarise(sd(ppe))

ggsave("assignments/keys/assignment_4_descr_locale.png")


####################################
#  2. Grade-band levels
####################################

## 2.2 ANOVA test
fit1 <- lm(ppe ~ level, data=nerds)
anova(fit1)

## 2.3 Categorical OLS, set elementary schools as reference category
fit2 <- lm(ppe ~ relevel(level, ref="Elementary"), data=nerds)
summary(fit2)

modelsummary(fit2, 
             stars=T,
             fmt=2,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c(`relevel(level, ref = "Elementary")Early Ed` = "Early Education",
                             `relevel(level, ref = "Elementary")High` = "High School",
                             `relevel(level, ref = "Elementary")Middle` = "Middle School"),
             output = "assignments/keys/assignment_4_level.docx")
                             
## 2.4 Compare HS to others

# Can approach this one of two ways: either conduct a series of pairwise t-tests with each of the other three categories, 
# using HS as the reference category OR create a new indicator for HS and test it against combined "non-HS" group
fit3 <- lm(ppe ~ relevel(level, ref="High"), data=nerds)
summary(fit3)

nerds <- mutate(nerds, high_school = ifelse(level=="High", 1, 0))

fit4 <- lm(ppe ~ high_school, data=nerds)
summary(fit4)

modelsummary(list(fit3, fit4),
             stars=T,
             fmt=2,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c(`relevel(level, ref = "High")Early Ed` = "Early Education",
                             `relevel(level, ref = "High")Elementary` = "Elementary School",
                             `relevel(level, ref = "High")Middle` = "Middle School",
                             "high_school" = "High School"),
             output = "assignments/keys/assignment_4_highschool.docx")


####################################################
## 3. Multiple regression with categorical variables
####################################################

## 3.1 Correlation matrix

# Drop the identifier and categorical variables
nerds3 <- select(nerds, -c(schoolname, ncesid, ncesdistid_geo, distname, high_school, level, locale))

datasummary_correlation(nerds3,
                        fmt=3, 
                        output="assignments/keys/assignment_4_corr_matrix.docx")


## 3.2 Multiple regression

# going to change factor level to make elementary the first category
nerds$level <- relevel(nerds$level, ref="Elementary")

fit5 <- lm(ppe ~ frpl, data=nerds)
fit6 <- lm(ppe ~ frpl + level + enroll + locale + sesavgall + unempavgall + baplusavgall, data=nerds)
summary(fit5)
summary(fit6)

### Show only the question predictor, and including rows indicating additional covariate adjustments

# Create a mini-dataframe containing the information you want to have in the added row
row <- tribble(~term, ~Bivariate, ~Multivariate,
               "<b>Covariates?</b>", "<b>No</b>", "<b>Yes</b>")
# Tell which position it should go
attr(row, 'position') <- c(5)

modelsummary(list(fit5, fit6), 
             stars=T,
             fmt=2,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_omit = "enroll|sesavgall|locale|unempavgall|baplusavgall|level",
             coef_rename = c("frpl" = "Prop. receiving FRPL (0/1)"),
             add_rows = row,
             notes= c("Cells report coefficients and heteroscedastic-robust standard errors in parentheses. Each observation is one school. Covariates in Model 2 are grade-band level of school, total enrollment, geographic locale, avg. SES status, unemployment rate, and four-year college degree holding rate."),
             output = "assignments/keys/assignment_4_MR.docx")


# Use the margins package and show at schooling-level values
df <- margins(fit6, at = 
                    list(level = c("Elementary", "Middle", "High")))

# Use prototypical values in resulting dataset to show results
ggplot(data=df, aes(x=frpl, y=fitted, color=as.factor(level))) + 
  geom_smooth(method='lm', se=F) +
  xlab("Proportion students receiving FRPL") + ylab("Predicted PPE") +
  scale_color_discrete(name = "Grade Level") +
  theme_minimal(base_size=16)

ggsave("assignments/keys/assignment_4_prototypical.png")
