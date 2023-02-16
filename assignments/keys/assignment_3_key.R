#######################################################################################################
# EDUC 643 Assignment 3 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 2/3/23
### Last update: 2/16/23
### Inputs: nerds.csv
### Purpose: load in data, understand structure, select covariates, fit MR model and produce results
#######################################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)

# Identify directory location of file
i_am("assignments/keys/assignment_3_key.R")


####################################
#  Read in the data 

nerds <- read.csv(here("data/nerds.csv"))

# 1.1 Focus on continuous variables
nerds2 <- select(nerds, -c(schoolname, ncesid, ncesdistid_geo, distname, level, locale))

summary(nerds2)

# 1.2 Construct correlation matrix and/or heatmap

# Correlation matrix
datasummary_correlation(nerds2,
                        fmt=3, 
                        output="assignments/keys/assignment_3_corr_matrix.docx")

# Heat map
cormat <- round(cor(nerds2),3)
cormat[upper.tri(cormat)] <- NA

melt_corm <- reshape2::melt(cormat, na.rm=T)
heatmap <- ggplot(data = melt_corm, aes(Var2, Var1, fill = value)) +
              geom_tile(color = "white") +
              scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
              ylab("") + xlab("") +
              theme_minimal(base_size = 12)

heatmap

ggsave("assignments/keys/assignment_3_heatmap.png", width=10, height=6)

# 1.5 Multiple regression
fit1 <- lm(ppe ~ frpl, data=nerds)
fit2 <- lm(ppe ~ frpl + enroll, data=nerds)
fit3 <- lm(ppe ~ frpl + enroll + lninc50avgall + baplusavgall + unempavgall + snapavgall + sesavgall, data=nerds)
fit4 <- lm(ppe ~ frpl + enroll + lninc50avgall + baplusavgall + unempavgall + snapavgall, data=nerds)

# I'm adding this custom function to make my coefficients have a comma and round to the second digit after the decimal
# This isn't necessary for your assignment
f <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")

modelsummary(list(fit1, fit2, fit3, fit4),
             stars=T,
             vcov = "robust",
             fmt = f,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|Std.Err",
             coef_rename = c("frpl" = "Receiving FRPL (0-1)", 
                             "enroll" = "Total enrollment",
                             "lninc50avgall" = "Median income (log)",
                             "baplusavgall" = "BA+ holders (0-1)",
                             "unempavgall" = "Unemployment rate (0-1)",
                             "snapavgall" = "SNAP receipt rate (0-1)",
                             "sesavgall" = "SES composite"),
             notes=c("Cells report coefficients and heteroscedastic-robust standard errors in parentheses. Each observation is one school."),
             output = "assignments/keys/assignment_3_table2.docx"
             )
