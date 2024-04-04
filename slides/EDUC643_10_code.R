############################################################################
# Categorical predictors
## Created by: David Liebowitz
### First created: 2/1/23
### Last update: 2/13/24
### inputs: dibels.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, modelsummary)


i_am("slides/EDUC643_10_code.R")

###########################################
### Data management
###########################################

# Import the data 
dibels <- read.csv(here("data/dibels.csv"))

# Understanding data structure
str(dibels)

# How many unique schools are represented?
length(unique(dibels$sch_deid))

# How many total students contribute test-scores?
sum(dibels$tr_ts)

# Will need to have a long version of the data -- Not necessary to know how to do
dibels_long <- dibels %>% 
                pivot_longer(
                  cols = c("y1_boy_mean", "y1_moy_mean",      # cols indicate which columns include the same variable stored across multiple columns
                           "y2_boy_mean", "y2_moy_mean"),     # these are what vary within-unit and will be what pivot into long form
                  names_to = "period",                        # this tells R what to call the new variable that indexes the group the observation belongs to
                  names_pattern = "(.*)_mean",                # this tells R how to extract the name of the original column and turn it into the new value it will take on in the names_to column
                  values_to = "mean_orf")                     # this tells what to call the newly created variable with the values from the original columns

# Make period a factor
dibels_long$period <- factor(dibels_long$period)

# Drop pre/post
dibels_long <- select(dibels_long, -c(pre, post))

# Create dummy for post
dibels_long <- mutate(dibels_long,
                      post = ifelse(period=="y1_boy" | 
                                      period=="y1_moy", 0, 1))

# Convert school_titlei into a collapsed and sensible categorical
table(dibels_long$school_titlei)

dibels_long <- dibels_long %>%
  mutate(title1 = case_when(school_titlei=="Missing" ~ "Missing",
                            school_titlei=="Not a Title I school" ~ "Not Title I",
                            school_titlei=="Title I schoolwide eligible-Title I targeted assistance program" |
                              school_titlei=="Title I schoolwide eligible school-No program" |
                              school_titlei=="Title I schoolwide school" ~ "Title I schoolwide",
                            school_titlei=="Title I targeted assistance eligible school-No program" |
                              school_titlei=="Title I targeted assistance school" ~ "Title I targeted"))

# Check the new values; note that exclude=NULL reports the number of NA values as well
table(dibels_long$title1, exclude=NULL)

# Hand construct dummies of Title I
dibels_long <- dibels_long %>%
  mutate(title1_school = ifelse(title1=="Title I schoolwide", 1, 0)) %>%
  mutate(title1_target = ifelse(title1=="Title I targeted", 1, 0)) %>%
  mutate(title1_miss = ifelse(title1=="Missing", 1, 0))

# Or just create a factor and R will figure it out
dibels_long$title1 <- factor(dibels_long$title1)

###########################################
### Descriptives
###########################################

# Mean comparisons
mean(dibels$pre)
mean(dibels$post)

### Distribution displays
# Pre pandemic onset
boxplot(dibels$pre,
        ylim =c(0,170))
# Post pandemic onset
boxplot(dibels$post,
        ylim =c(0,170))

# Overlapping density plots
plot(density(dibels$pre), main=" ", sub=NULL, ylim=range(0,0.011))
lines(density(dibels$post), col="red")
legend(150, .008, legend=c("pre", "post"), fill=c("black", "red"))

################################################
### Analysis
################################################

# Default two-sample t-test
t.test(dibels$pre, dibels$post) 

# Can make stronger assumptions for higher degree of precision in estimates
t.test(dibels$pre, dibels$post,
       paired=T, var.equal=F)

# To make formatted data structure displays (not necessary to know)
library(DT)
datatable(dibels[,c(1:4, 13:14)], fillContainer = FALSE, 
          options = list(pageLength = 9)) %>%
              formatRound("y1_boy_mean", digits=1) %>%
              formatRound("y1_moy_mean", digits=1) %>%
              formatRound("pre", digits=1) %>%
              formatRound("post", digits=1)

# Fit linear regression on post dummy
fit1 <- lm(mean_orf ~ post, data=dibels_long)
summary(fit1)

# Visualize how the categorical predictor regression works using the jitter command (not necessary to know how to do)
ggplot(dibels_long, aes(y=mean_orf, x=post, color=post)) +
  geom_jitter() +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")

##### Changing the reference category ####
# First, need to create a new dummy variable called "pre", coded as one if test is prior to pandemic onset
dibels_long <- mutate(dibels_long,
                      pre = ifelse(period=="y1_boy" | 
                                     period=="y1_moy", 1, 0))

# Fit the new model
fit2 <- lm(mean_orf ~ pre, data=dibels_long)
summary(fit2)

# Show the boxplot of within- and between-group variance
ggplot(dibels_long, aes(x=period, y=mean_orf, color=period, fill=period, alpha=0.4)) +
  geom_boxplot() +
  xlab("Period") + ylab("Mean ORF score") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")

# Estimate the model with each wave entered as separate dummy
# Note that R automatically turns factors into a series of dummies
fit3 <- lm(mean_orf ~ period, dibels_long)
summary(fit3)

### Changing the reference category
# I can specify directly in my call which group to serve as reference
summary(lm(mean_orf ~ relevel(period, ref="y2_boy"), data=dibels_long))

# Note that you can also do this by releveling the actual variable

## Bonferroni correction for multiple hypothesis testing
pairwise.t.test(dibels_long$mean_orf, dibels_long$period,
                p.adjust.method = "bonferroni")

# Within and between group differences with violin plots
ggplot(dibels_long, aes(x=period, y=mean_orf, fill=period, color=period, alpha=0.4)) +
  geom_violin() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")

#######   ANOVA    ########
# Fit an ANOVA to test whether there is significant between-group variation
anova(fit3)

###### Multiple regression w/ categoricals  ######

# Add another categorical and another continuous
fit4<-lm(mean_orf ~ period + as.factor(grade) + 
           school_enroll, data=dibels_long)
summary(fit4)

######   ANCOVA     ######

# Test whether the categoricals are different by group
anova(fit4)

# Compare the model fit between an ANOVA and ANCOVA
anova(fit3, fit4)

#############################################################
###                  Result presentation
#############################################################

# Show the pre/post and multi-wave estimates
modelsummary(list(fit1, fit3),
             stars=T,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c("post" = "Post-Pandemic Onset", 
                             "periody1_moy" = "Winter 2020",
                             "periody2_boy" = "Fall 2020",
                             "periody2_moy" = "Winter 2021"))

# Plot coefficients from a model fit
# These are not particularly informative here, but can be quite useful in other settings
modelplot(fit3,
          coef_rename = c("(Intercept)" = "Fall 2019 (Intercept)", 
                          "periody1_moy" = "Winter 2020",
                          "periody2_boy" = "Fall 2020",
                          "periody2_moy" = "Winter 2021"),
          vcov = "robust") +
          coord_flip() +
          theme_minimal(base_size = 16)

### Display a continuous against a continuous, adjusting for ONE categorical only

# The raw relationship
ggplot(data=dibels_long, aes(x=school_enroll, y=mean_orf)) +
  geom_point() + 
  geom_smooth(method='lm') +
  theme_minimal(base_size=16)

# Now show it adjusting for the ONE categorical
ggplot(dibels_long, aes(x=school_enroll, y=mean_orf, color=period)) +
  geom_point() + 
  geom_smooth(method='lm') +
  theme_minimal(base_size=16)

### Display a continuous against a continuous, adjusting for multiple covariates and displaying variation by ONE categorical

# First, estimate the margins at the relevant values of the crucial categorical
df3 <- margins::margins(fit4,
                        at = list(period=c("y1_boy", "y1_moy", 
                                           "y2_boy", "y2_moy")))

# Then use prototypical values in resulting dataset to show results
ggplot(data=df3, aes(x=school_enroll, y=fitted,color=period)) + 
  geom_smooth(method='lm') +
  xlab("School Enrollment") + ylab("Predicted ORF") +
  scale_color_discrete(name = "Period",
                       breaks=c("y1_boy", "y1_moy", 
                                "y2_boy", "y2_moy"),
                       labels=c("Fall 2019","Winter 2020",
                                "Fall 2020", "Winter 2021")) +
  theme_minimal(base_size=16)

## Show the multiple regression results alongside single categorical predictor

# Show all coefficients
modelsummary(list(fit3, fit4),
             stars=T,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_rename = c("periody1_moy" = "Winter 2020",
                             "periody2_boy" = "Fall 2020",
                             "periody2_moy" = "Winter 2021",
                             "as.factor(grade)2" = "2nd Grade",
                             "as.factor(grade)3" = "3rd Grade",
                             "as.factor(grade)4" = "4th Grade",
                             "as.factor(grade)5" = "5th Grade",
                             "school_enroll" = "School Enrollment (#)"))

### Show only the question predictor, and including rows indicating additional covariate adjustments

# Create a mini-dataframe containing the information you want to have in the added row
row <- tribble(~term, ~Bivariate, ~Multivariate,
               "<b>Covariates?</b>", "<b>No</b>", "<b>Yes</b>")
# Tell which position it should go
attr(row, 'position') <- c(9)

# Re-generate the table
modelsummary(list(fit3, fit4),
             stars=T,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_omit = "grade|school_enroll",
             coef_rename = c("periody1_moy" = "Winter 2020",
                             "periody2_boy" = "Fall 2020",
                             "periody2_moy" = "Winter 2021"),
             add_rows = row
              )
