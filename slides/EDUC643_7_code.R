############################################################################
# Multiple regression
## Created by: David Liebowitz
### First created: 1/23/23
### Last update: 1/31/23
### inputs: male_do_eating.sav
######################################################

# Load necessary packages
library(pacman)
p_load(here, haven, tidyverse, modelsummary, margins)


i_am("slides/EDUC643_7_code.R")

# Import the data and implement previously identified data-cleaning steps
do <- read_spss(here("data/male_do_eating.sav")) %>% 
  select(OE_frequency, EDEQ_restraint, EDS_total,
         BMI, age_year, income_group) %>%
  mutate(EDS_total = ifelse(EDS_total==-99, NA, EDS_total)) %>%
  drop_na()

# Going to add an id variable
do <- rownames_to_column(do, "id")

#################################################
###  First, estimate the bivariate relationship
################################################

### Some univariate stats

summary(do$OE_frequency)
sd(do$OE_frequency)
summary(do$EDEQ_restraint)

### Some univariate displays

OE <- ggplot(do, aes(OE_frequency)) +
  geom_histogram() +
  theme_minimal(base_size = 16)
EDEQ <- ggplot(do, aes(EDEQ_restraint)) +
  geom_histogram() +
  theme_minimal(base_size = 16)

gridExtra::grid.arrange(OE, EDEQ, ncol=2)

### Some bivariate statistics
cor(do$OE_frequency, do$EDEQ_restraint)


# Bivariate visualization
lm_plot <- ggplot(do, aes(x=EDEQ_restraint, y=OE_frequency)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Dietary restraint index") +
  ylab("Overeating frequency") +
  theme_minimal(base_size = 16)

lm_plot

# Estimate the bivariate relationship
fit <- lm(OE_frequency ~ EDEQ_restraint, data=do)
summary(fit)

# Examine the bivariate regression assumptions

do$predict <- predict(fit)
do$resid <- resid(fit)
do$stu_resid <- rstudent(fit)

# Plot the raw residuals
ggplot(do, aes(x = resid)) + 
  geom_histogram(binwidth = 1) +
  theme_minimal(base_size = 16)

# Residuals v fitted plot
ggplot(do, aes(x = predict, y = stu_resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Studentized Residuals") + xlab("Fitted values") +
  theme_minimal(base_size = 16)


#################################################
###  Now, multiple regression
################################################

### The relationship between OE_frequency and BMI
oe_bmi <- ggplot(do, aes(x=BMI, y=OE_frequency)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("BMI") +
  ylab("Overeating frequency") +
  theme_minimal(base_size = 16)

oe_bmi

# And the bivariate relationship
summary(lm(OE_frequency ~ BMI, data=do))

# Finally, the multivariate relationship
fit2 <- lm(OE_frequency ~ EDEQ_restraint + BMI, data=do)

fit2

#####################################################################################
## You do not need to follow the below, but if you'd like to see how to 
## create lines of best fit by group, it is as follows
## We'll explicitly learn how to do this in Unit 4

# First, I'm creating the simulated data
set.seed(1234)
library(MASS)
### build the g1
mu <- c(0,0)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g1 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g1$group <- c("Hi GDP")

### build the g2
mu <- c(3,3)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g2 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g2$group <- c("Mid GDP")

### build the g3
mu <- c(6,6)
sigma <- rbind(c(2,-0.7),c(-0.7,2) )
g3 <- as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
g3$group <- c("Lo GDP")

# the combined data of all three groups
df <- rbind(g1,g2,g3)

# Changing the variable names for display purposes
df <- df %>% mutate(`% GDP on Health Care` = V1 + 5.2)
df <- df %>% rename(`Early mortality` = V2)

# Plotting the bivariate relationship
ggplot(df, aes(`% GDP on Health Care`, `Early mortality`)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  theme_minimal(base_size=16)

# Plotting the bivariate relationship with Simpson's Paradox revealed
ggplot(df) +
    geom_point(aes(x=`% GDP on Health Care`, y=`Early mortality`, col=group)) +
    geom_smooth(aes(x=`% GDP on Health Care`, y=`Early mortality`, col=group), method='lm', se=F) +
    geom_smooth(aes(x=`% GDP on Health Care`, y=`Early mortality`), method='lm', se=F, color="gray") +
    theme_minimal(base_size=16) + 
    annotate('text', x=10.7, y=-3, label="Hi GDP", color="#F8766D", size=5) +
    annotate('text', x=13, y=-1, label="Mid GDP", color="#619CFF", size=5) +
    annotate('text', x=15, y=2.5, label="Low GDP", color="#00BA38", size=5) +
    theme(legend.position="none")

###############################################################################################################

# Preparing a nice regression table -- on screen
modelsummary(list(fit, fit2),
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"))

# Preparing a nice regression table -- output to .doc
modelsummary(list(fit, fit2),
             stars=T,
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE",
             coef_rename = c("EDEQ_restraint" = "Dietary Restraint Index (0-6)"),
             output = "slides/do_multiple.docx")

## Creating a prototypical plot

# Estimate the relationship in the mtcars data
car <- lm(mpg ~ hp + wt, data=mtcars)

# Use the margins package and define prototypical values
df2 <- margins::margins(car, at = list(wt = c(2,3,4)))

# Use prototypical values in resulting dataset to show results
proto <-  ggplot(data=df2, aes(x=hp, y=fitted, color=as.factor(wt))) + 
  geom_smooth(method='lm', se=F) +
  xlab("Horsepower") + ylab("Predicted MPG") +
  scale_color_discrete(name = "Weight",
                       breaks=c(2,3,4),
                       labels=c("2,000 lbs","3,000 lbs","4,000 lbs")) +
  theme_minimal(base_size=16)

proto

#####################################
## Creating a correlation matrix 
######################################

# First, limit our data to only the variables of interest
cordat <- do %>%
  dplyr::select(OE_frequency, EDEQ_restraint, EDS_total,
                BMI, age_year, income_group)

## Note, when you load the MASS package (to generate the simulated data) it has its own function involving select
## If you try to use the select function after the MASS package, you'll get an error
## Unless you specifically tell R you want to use the select function from dplyr (which is what I'm doing above with the dplyr:: line)

# I'm going to rename some, so they fit nicely on the screen
cordat <- cordat %>%
  rename(OE = OE_frequency,
         EDEQ = EDEQ_restraint,
         EDS = EDS_total,
         Age = age_year,
         Income=income_group)

# Income was initially recorded as a categorical variable
# It's generally interval, and I'm going to re-code it as numeric
# In fact, we probably would want to use the Spearman correlation coefficient
# But for now, it's ok
cordat$Income = as.numeric(as.character(cordat$Income))

#Produce the correlation matrix
datasummary_correlation(cordat,
                        fmt = 3,
                        notes = "Notes: cells report Pearson correlation coefficients.")

### Don't expect you to know how to do this ####
# Can also produce a heat map

# First, I need to create an object that contains the correlation coefficients
cormat <- round(cor(cordat),3)
cormat[upper.tri(cormat)] <- NA

# The melt command is a quick way to reshape the data from wide-to-long, which is necessary here
melt_corm <- data.table::melt(cormat, na.rm=T)
ggplot(data = melt_corm, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal(base_size = 16)
