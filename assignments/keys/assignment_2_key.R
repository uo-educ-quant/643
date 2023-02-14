##########################################################################################
# EDUC 643 Assignment 2 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 1/31/23
### Last update: 2/11/23
### Inputs: nerds.csv
### Purpose: load in data, understand structure, fit MR model
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)

# Identify directory location of file
i_am("assignments/keys/assignment_2_key.R")


####################################
#  Read in the data 

nerds <- read.csv(here("data/nerds.csv"))

# 1.2 Examine South Eugene High School
sehs <- filter(nerds, ncesid==410474000573)

summary(sehs$ppe)
summary(sehs$frpl)

all <- ggplot(nerds, aes(frpl, ppe)) + 
            geom_point(color='cornflowerblue', alpha=0.3) +
            geom_smooth(method='lm', color='black') +
            labs(x = "Free- or reduced-price lunch rate",
                y = "Per-pupil expenditure ($)") +
            theme_minimal(base_size=12)

all

all + 
      geom_point(data=sehs, aes(frpl, ppe), color="deeppink", size=2.5) +
      annotate('text', x=0.25, y=17000, label="S. Eugene HS", color='deeppink')

ggsave("assignments/keys/assignment_2_sehs.png")

# 1.3 Examine residuals

# Fit relationship
fit <- lm(ppe ~ frpl, data=nerds)

# Extract residuals from model fit
nerds$predict <- predict(fit)
nerds$raw_resid <- resid(fit)
nerds$std_resid <- rstandard(fit)
nerds$stu_resid <- rstudent(fit)

# Examine overall normality of residuals
## Going to use studentized residuals to avoid missing outlying values
## that are overly influential

## Histogram
hist(nerds$stu_resid,
     xlab="Studentized residuals",
     main="")

#### Export base R plot directly from code
# 1. Open png file
png("assignments/keys/assignment_2_hist.png")
# 2. Create the plot
hist(nerds$stu_resid,
     xlab="Studentized residuals",
     main="")
# 3. Close the file
dev.off()

## QQ plot
qq <- ggplot(nerds) + 
  stat_qq(aes(sample=stu_resid)) +
  geom_abline(color='cornflowerblue') +
  theme_minimal(base_size=12)

qq

ggsave("assignments/keys/assignment_2_qq.png")

## Fitted vs. residuals
ggplot(nerds, aes(x = predict, y = raw_resid)) + 
    geom_point() +
    geom_hline(yintercept = 0, color = "red", linetype="dashed") +
    ylab("Raw Residuals") + xlab("Fitted values") +
    theme_minimal(base_size = 16)

ggsave("assignments/keys/assignment_2_resid_fitted.png")
