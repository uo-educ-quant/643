############################################################################
# Estimating bivariate relationship and check assumptions
## Created by: David Liebowitz
### First created: 12/15/22
### Last update: 1/23/23
### inputs: male_do_eating.sav
######################################################

# Load necessary packages
library(pacman)
p_load(here, haven, tidyverse, modelsummary)


i_am("slides/EDUC643_1_code.R")

# Import the data and implement previously identified data-cleaning steps
do <- read_spss(here("data/male_do_eating.sav")) %>% 
  select(OE_frequency, EDEQ_restraint, EDS_total,
         BMI, age_year, income_group) %>%
  mutate(EDS_total = ifelse(EDS_total==-99, NA, EDS_total)) %>%
  drop_na()

# Going to add an id variable
do <- rownames_to_column(do, "id")

################################
###  Bivariate relationship
################################

# Plot bivariate relationship

lm_plot <- ggplot(do, aes(x=EDEQ_restraint, y=BMI)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Dietary restraint index") + 
  theme_minimal(base_size = 16)

lm_plot

# Fit regression
fit <- lm(BMI ~ EDEQ_restraint, data=do)

# As an alternative to summary, can use tidy from the `broom` package to more easily manipulate results post-estimation
# Automatically returns a dataframe
tidy(fit)

# Can also use good old summary:
summary(fit)


###################################
### Working with residuals
###################################

# Extract raw residuals and fitted/predicted values
do$predict <- predict(fit)
do$resid <- residuals(fit)

# Extract standardized and studentized residuals
do$std_resid <- rstandard(fit)
do$stu_resid <- rstudent(fit)

# Produce some summary statistics for raw and standardized residuals
summary(do$resid)
sd(do$resid)

summary(do$std_resid)
sd(do$std_resid)

# Generate residuals vs. fitted plot
ggplot(do, aes(x = predict, y = resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Raw Residuals") + xlab("Fitted values") +
  theme_minimal(base_size = 16)

##############
## Assessing residual normality

# via boxplot
boxplot(rstudent(fit))

# via histogram
ggplot(do, aes(x = resid)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal(base_size = 16)

# via Q-Q plot
ggplot(do) + 
  stat_qq(aes(sample=stu_resid)) +
  geom_abline(color=blue) +
  theme_minimal(base_size = 16)


####################################################################################
## Some solutions to violations of homoscedasticity and independence assumptions
####################################################################################

# Use modelsumamry to report different types of standard errors
modelsummary(fit, 
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"),
             vcov =  list("iid", "robust"))

### Going to create artifical clusters:
do <- do %>% mutate(city = case_when(predict<=25 & stu_resid>0 ~ "San Francisco",
                                     predict<=25 & stu_resid<=0 ~ "Portland",
                                     predict >25 & predict <=28 & stu_resid>0 ~ "Seattle",
                                     predict >25 & predict <=28 & stu_resid<=0 ~ "Los Angeles",
                                     predict >28 & stu_resid>0 ~ "Eugene",
                                     predict >28 & stu_resid<=0 ~ "Oakland"))

# Then fit a regression in which I include these clusters as predictors
lm_city <- lm(BMI ~ EDEQ_restraint + as.factor(city), data=do)

# Finally, implement a post-hoc correction to the standard errors
modelsummary(lm_city,
             stars=T,
             coef_omit = "city",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"),
             vcov =  list("iid", ~ city))

## As an alternative, can use estimatr::lm_robust
## You'll see that to get the exact same standard errors, you need to specify se_type== "HC3"
hetero <- estimatr::lm_robust(BMI ~ EDEQ_restraint, data=do)
summary(hetero)

cluster <- estimatr::lm_robust(BMI ~ EDEQ_restraint + as.factor(city), data=do)
summary(cluster)
