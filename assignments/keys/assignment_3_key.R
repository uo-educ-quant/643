##########################################################################################
# EDUC 643 Assignment 3 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 2/3/23
### Last update: 2/14/23
### Inputs: nerds.csv
### Purpose: load in data, understand structure, fit MR model
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)
library(reshape2)

# Identify directory location of file
i_am("assignments/keys/assignment_3_key.R")


####################################
#  Read in the data 

nerds <- read.csv(here("data/nerds.csv"))


# 1.1 Focus on continuous variables
nerds2 <- select(nerds, -c(schoolname, ncesid, ncesdistid_geo, distname, level, locale))

summary(nerds2)

# 1.2 Construct correlation matrix and/or heatmat

# Correlation matrix
datasummary_correlation(nerds2,
                        fmt=3)

# Heat map
cormat <- round(cor(nerds2),3)
cormat[upper.tri(cormat)] <- NA

melt_corm <- melt(cormat, na.rm=T)
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
fit2 <- lm(ppe ~ frpl + enroll + sesavgall + lninc50avgall + baplusavgall + unempavgall + snapavgall, data=nerds)

summary(fit)
