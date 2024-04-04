############################################################################
# Interactions
## Created by: David Liebowitz
### First created: 2/13/23
### Last update: 2/21/24
### inputs: dibels_long.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, modelsummary)


i_am("slides/EDUC643_13_code.R")

###########################################
### Data management
###########################################

# Import the data 
dibels_long <- read.csv(here("data/dibels_long.csv"))

# Clean up the Title I flag
dibels_long <- dibels_long %>%
  mutate(title1 = case_when(school_titlei=="Missing" ~ "Missing",
                            school_titlei=="Not a Title I school" ~ "Not Title I",
                            school_titlei=="Title I schoolwide eligible-Title I targeted assistance program" |
                              school_titlei=="Title I schoolwide eligible school-No program" |
                              school_titlei=="Title I schoolwide school" ~ "Title I schoolwide",
                            school_titlei=="Title I targeted assistance eligible school-No program" |
                              school_titlei=="Title I targeted assistance school" ~ "Title I targeted"))

dibels_long$title1 <- factor(dibels_long$title1,
                             levels=c("Not Title I", "Title I schoolwide", "Title I targeted", "Missing"))

#############################################
### Prior analyses
#############################################

fit3 <- lm(mean_orf ~ period, dibels_long)
fit4 <- lm(mean_orf ~ period + as.factor(grade) + school_enroll, data=dibels_long)

# Add row indicating presence of covariates
row <- tribble(~term, ~Bivariate, ~Multivariate,
               "<b>Covariates?</b>", "<b>No</b>", "<b>Yes</b>")
attr(row, 'position') <- c(9)

# Produce table
modelsummary(list(fit3, fit4),
             stars=T,
             vcov = "robust",
             gof_omit = "Adj.|AIC|BIC|Log|RMSE|RSE|Std.Err",
             coef_omit = "grade|school_enroll",
             coef_rename = c("periody1_moy" = "Winter 2020",
                             "periody2_boy" = "Fall 2020",
                             "periody2_moy" = "Winter 2021"),
             add_rows = row,
             notes=c("Cells report coefficients and heteroscedastic-robust standard errors in parentheses. Each observation is a school-grade-test value. Covariates include grade-level and total school enrollment."))

# Visualize main effects model
df4 <- margins::margins(fit4,
                        at = list(period=c("y1_boy", "y1_moy", 
                                           "y2_boy", "y2_moy")))

# Use prototypical values in resulting dataset to show results
proto1 <- ggplot(data=df4, aes(x=frpl_prop, y=fitted, color=period)) + 
  geom_smooth(method='lm') +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  ylim(35, 110) +
  scale_color_discrete(name = "Period",
                       breaks=c("y1_boy", "y1_moy", 
                                "y2_boy", "y2_moy"),
                       labels=c("Fall 2019","Winter 2020",
                                "Fall 2020", "Winter 2021")) +
  theme_minimal(base_size=16)

proto1

######################################
### Interactions
######################################

######################################
##  Categorical X continuous

# Period as predictor
summary(lm(mean_orf ~ period, dibels_long))

# *FRPL_PROP* as predictor
summary(lm(mean_orf ~ frpl_prop, dibels_long))

# Both
summary(lm(mean_orf ~ period + frpl_prop, dibels_long))

# Include interaction
summary(lm(mean_orf ~ period * frpl_prop, dibels_long))

# Generate fitted values, including interaction
fit5 <- lm(mean_orf ~ period * frpl_prop, data=dibels_long)
df5 <- margins::margins(fit5,
                        at = list(period=c("y1_boy", "y1_moy", 
                                           "y2_boy", "y2_moy")))

# Use prototypical values in resulting dataset to show results
proto2 <- ggplot(data=df5, aes(x=frpl_prop, y=fitted, color=period)) + 
  geom_smooth(method='lm') +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  ylim(35, 110) +
  scale_color_discrete(name = "Period",
                       breaks=c("y1_boy", "y1_moy", 
                                "y2_boy", "y2_moy"),
                       labels=c("Fall 2019","Winter 2020",
                                "Fall 2020", "Winter 2021")) +
  theme_minimal(base_size=16)

proto2

## Can also just sub-set data by groups
# Fall 2019
summary(lm(mean_orf ~ frpl_prop, data=subset(dibels_long, period=="y1_boy")))

# Winter 2021
summary(lm(mean_orf ~ frpl_prop, data=subset(dibels_long, period=="y2_moy")))


###################################
### Categorical x categorical

## Review categories
table(dibels_long$title1, exclude=NULL)
dibels_long %>% group_by(title1) %>% summarize(mean= mean(mean_orf))

# Title I as predictor
summary(lm(mean_orf ~ title1, dibels_long))

# Both
summary(lm(mean_orf ~ period + title1, dibels_long))

# Interaction
summary(lm(mean_orf ~ period * title1, dibels_long))

## Visualize results
fit6 <- lm(mean_orf ~ period * title1, dibels_long)

df6 <- margins::margins(fit6,
                        at = list(period=c("y2_boy", "y2_moy"),
                                  title1 = c("Not Title I", "Title I schoolwide", 
                                             "Title I targeted", "Missing")))

# Show results for each category
categ <- ggplot(data=df6, 
                aes(x=period, y=fitted, 
                    ymin=fitted-1.96*se.fitted, ymax=fitted+1.96*se.fitted, 
                    group=title1, color=title1)) + 
  geom_pointrange(position=position_dodge(width=0.2)) +
  ylab("Predicted ORF") + xlab(" ") +
  scale_x_discrete(labels= c("y2_boy" = "Fall 2020",
                             "y2_moy" = "Winter 2021")) +
  ylim(0, 110) +
  theme_minimal(base_size=16) +
  theme(legend.title = element_blank())

categ

# Can add connecting lines
categ + geom_line(position=position_dodge(width=0.2))



###################################
### Continuous x continuous

# School enrollment as predictor
summary(lm(mean_orf ~ school_enroll, dibels_long))

# Both
summary(lm(mean_orf ~ frpl_prop + school_enroll, dibels_long))

# Interaction
summary(lm(mean_orf ~ frpl_prop * school_enroll, dibels_long))

# Prototypical values?
quantile(dibels_long$school_enroll, probs = seq(0, 1, 0.1))


# Visualizing -- select prototypical values
fit7 <- lm(mean_orf ~ frpl_prop * school_enroll, dibels_long)

df7 <- margins::margins(fit7,
                        at = list(school_enroll=c(120, 326, 600)))

# Use prototypical values in resulting dataset to show results
cont <- ggplot(data=df7, aes(x=frpl_prop, y=fitted, 
                             color=as.factor(school_enroll))) +
  geom_smooth(method='lm') +
  xlab("Proportion receiving FRPL") + ylab("Predicted ORF") +
  ylim(35, 100) +
  scale_color_discrete(name = "School Enrollment",
                       breaks=c(120, 326, 600),
                       label=c("~10th pctile (120 stu.)", 
                               "Median (326 stu.)", 
                               "~90th pctile (600 stu.)")) +
  theme_minimal(base_size=16)

cont