############################################################################
# Importing disordered eating data and estimating bivariate relationship
## Created by: David Liebowitz
### First created: 12/15/22
### Last update: 1/9/23
### inputs: male_do_eating.sav
######################################################

# Load necessary packages
library(pacman)
p_load(here, haven, tidyverse, modelsummary)


###This tells R where the script is located in relationship to the "root" directory of your project
# Using this command you can then use shortened versions of file pathways that will work across different users' systems
# A non-preferred alternative is to read the data in using the full filepath

i_am("slides/EDUC643_1_code.R")

# Let's first access the data
do <- read_spss(here("data/male_do_eating.sav")) %>%
        select(OE_frequency, EDEQ_restraint, EDS_total,
               BMI, age_year, income_group)



#############################################################
### Data Cleaning
#############################################################

# We start with some simple data-cleaning steps. You will get light exposure to these in this class, 
# and fully learn via intensive practice on applied research projects

# Understand data structure
head(do)
str(do)


# Check for missingness
sapply(do, function(x) sum(is.na(x)))

# Can also do this variable by variable
sum(is.na(do$OE_frequency))

# Things look good, but we'll focus on those cases that had complete  
# records for all variables
do <- do %>% drop_na()

# Can also do this for particular variables
do <- filter(do, !is.na(OE_frequency))

# Get a quick sense of some basic summary statistics
quantile(do$EDEQ_restraint)
summary(do$OE_frequency)
summary(do$EDS_total)

# Something looks funny with EDS_total, let's check it out
ggplot(data=do, aes(EDS_total)) +
    geom_bar()

# Re-code values of -99 as NA
do <- do %>% mutate(EDS_total = ifelse(EDS_total==-99, NA, EDS_total))

sum(is.na(do$EDS_total))

do <- filter(do, !is.na(EDS_total))


#######################################################
### Visualization of bivariate relationship
#######################################################

lm_plot <- ggplot(data=do, aes(x=EDEQ_restraint, y=BMI)) + 
              geom_point() +
              xlab("Dietary restraint index (0-6)") +
              theme_minimal(base_size = 16)


lm_plot

# Add line of best fit, the method is linear model (lm) and we are not including confidence intervals (se=F)
lm_plot +
  geom_smooth(method='lm', se=F)



####################################################################
## Bivariate regression
####################################################################

# Fitting a relationship using Ordinary Least Squares (OLS)
fit <- lm(BMI ~ EDEQ_restraint, data=do)
summary(fit)

# In addition to the handy display that summary returns, there are several other elements of the model fit that R stores under the hood

# Look at the structure of the object
str(fit)

# Can get descriptive values on residuals
quantile(fit$residuals)

# Can assign the fitted values to a variable in your original data set; NOTE: need to make sure that these are of same length!
do$predicted_BMI <- fit$fitted.values

# Can get the degrees of freedom for the mdoel
fit$df

# Can also extract the coefficients to use later on 
beta1_hat <- fit$coeff[2]  # <- we're extracting the second of the coefficients in the list; i.e., the slope on BMI

# Produce confidence intervals
tidy(fit, conf.int=T)

#Another way of producing CIs
confint(fit)

#############################################################
# Pass output to table
#############################################################

# Produce the table
modelsummary(fit, stars=T,
             gof_omit = "Adj.|AIC|BIC|Log",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"))

# Produce the table and export it to a Word file
modelsummary(fit, stars=T,
             gof_omit = "Adj.|AIC|BIC|Log",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"),
             output = "Tables/BMI_EDEQ.docx") # <- need to set up your directory structure to have a Tables file in your root directory

############################################################
# Calculate Pearson's product-moment correlation 
############################################################

# Standardize the variables
do <- do  %>% 
  mutate(BMI_std = (BMI - mean(BMI)) / sd(BMI))
do <- do %>% 
  mutate(EDEQ_std = 
           (EDEQ_restraint - mean(EDEQ_restraint)) / sd(EDEQ_restraint))

# Graph the standardized versions of the data
std <- ggplot(do, aes(EDEQ_std, BMI_std)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  theme_minimal(base_size=16)

std

# Calculate the correlation coefficient
cor(do$BMI, do$EDEQ_restraint)


