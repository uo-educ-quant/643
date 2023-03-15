#######################################################################################################
# EDUC 643 Assignment 5 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 3/7/23
### Last update: 3/15/23
### Inputs: nerds.csv
### Purpose: load in data, fit interaction model w/ categorical and continuous predictors and produce results
#######################################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)
library(margins)

# Identify directory location of file
i_am("assignments/keys/assignment_5_key.R")

#####################################
# Data management
#####################################

## Read in the data 
nerds <- read.csv(here("data/nerds.csv"))
str(nerds)

# Recoding level and locale to factor variables
nerds$level <- as.factor(nerds$level) %>%
  relevel(ref="Elementary")
nerds$locale <- as.factor(nerds$locale)


####################################
#  1. By Grade-Band Level
####################################

## 1.2 Fit models with main effects and interactions
fit1 <- lm(ppe ~ frpl, data=nerds)
fit2 <- lm(ppe ~ level, data=nerds)
fit3 <- lm(ppe ~ frpl * level, data=nerds)

modelsummary(list(fit1, fit2, fit3),
             stars=T,
             fmt=2,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c("levelEarly Ed" = "Early Education",
                             "levelMiddle" = "Middle School",
                             "levelHigh" = "High School",
                             "frpl" = "FRPL",
                             "frpl:levelEarly Ed" = "FRPL x Early Ed",
                             "frpl:levelHigh" = "FRPL x High",
                             "frpl:levelMiddle" = "FRPL x Middle"),
             output = "assignments/keys/assignment_5_level.docx")

df <- margins(fit3, 
              at = list(level=c("Elementary", "Middle", "High")))

ggplot(df, aes(x = frpl, y = fitted, color=level)) +
      geom_smooth(method='lm', se=F) +
      geom_ribbon(aes(ymin=fitted-1.96*se.fitted, ymax=fitted+1.96*se.fitted, fill=level), alpha=0.3, linetype=0) +
      xlab("Proportion students receiving FRPL") + ylab("Predicted PPE") +
      theme_minimal(base_size=16) +
      guides(fill=guide_legend("Grade Level"), color = "none")

ggsave("assignments/keys/assignment_5_prototypical.png")


####################################
#  2. By school size
####################################

## 2.2 Fit models with main effects and interactions

fit4 <- lm(ppe ~ frpl, data=nerds)
fit5 <- lm(ppe ~ enroll, data=nerds)
fit6 <- lm(ppe ~ frpl * enroll, data=nerds)

modelsummary(list(fit4, fit5, fit6),
             stars=T,
             fmt=2,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c("frpl" = "FRPL",
                             "enroll" = "Total enrollment",
                             "frpl:enroll" = "FRPL x Enrollment"),
             output = "assignments/keys/assignment_5_enroll.docx")

# Check for potential prototypical values
quantile(nerds$enroll, probs = seq(0,1, 0.1))

# 0%        10%        20%        30%        40%        50%        60%        70%        80%        90%       100% 
# 20.69512  137.76114  210.90183  293.37953  349.68112  405.30355  447.65983  521.41421  600.78221  808.04903 2866.08615

# So will use 140, 400 and 800 as roughly 10th, 50th and 90th percentiles

df2 <- margins(fit6, 
              at = list(enroll=c(140, 400, 800)))

ggplot(df2, aes(x = frpl, y = fitted, color=as.factor(enroll))) +
  geom_smooth(method='lm', se=F) +
  geom_ribbon(aes(ymin=fitted-1.96*se.fitted, ymax=fitted+1.96*se.fitted, fill=as.factor(enroll)), alpha=0.3, linetype=0) +
  xlab("Proportion students receiving FRPL") + ylab("Predicted PPE") +
  theme_minimal(base_size=16) +
  scale_fill_discrete(labels = c("~10th %ile (140 stu)", "~50th %ile (400 stu)", "~90th %ile (800 stu)")) +
  guides(fill=guide_legend("Enrollment"), color = "none")     ## color="none" prevents duplication of legend for both fill and color

ggsave("assignments/keys/assignment_5_prototypical2.png")




