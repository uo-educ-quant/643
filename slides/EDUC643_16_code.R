############################################################################
# Non-linearity
## Created by: David Liebowitz
### First created: 2/28/23
### Last update: 3/1/23
### inputs: pisa.csv ; oecd_2022.csv
######################################################

# Load necessary packages
library(pacman)
p_load(here, tidyverse, modelsummary)


i_am("slides/EDUC643_16_code.R")

################################
## Log transformations
################################

# Import the PISA data 
pisa <- read.csv(here("data/pisa.csv")) %>% select(c("Country", "total_spending", "read_score"))

# Convert the spending value into numeric by dropping the comma in the dollar amount
pisa$total_spending <- as.numeric(gsub(",", "", pisa$total_spending))

# Graph the bivariate relationship between total spending and reading scores
lin <- ggplot(pisa, aes(total_spending, read_score)) + 
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(size = .6, colour = "black")) +
  ylim(300, 600) +
  ylab("PISA reading score (2018)") + xlab("Total spending, age 6-15 ($)") + 
  scale_x_continuous(label=scales::comma)

lin

# Add the line of best fit
lin + 
  geom_smooth(method='lm', se=F)

# Test model assumptions

# Fit the model
fit <- lm(read_score ~ total_spending, data=pisa)

# Generate residual vs fitted plot
pisa$resid <- resid(fit)
pisa$fitted <- fitted(fit)
ggplot(pisa, aes(fitted, resid)) + geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype="dashed")


#################### Just for fun if you're curious how we made the flag points
# library(ggflags)
# library(countrycode)
# pisa$iso2 <- countrycode::countrycode(pisa$Country, "country.name", "iso2c")
# pisa$iso2 <- tolower(pisa$iso2)
# 
# flag <- ggplot(pisa, aes(total_spending, read_score)) + 
#   geom_point() + geom_flag(aes(country=iso2), show.legend=F) +
#   theme(panel.background = element_rect(fill = 'lightblue', colour = 'lightblue'),
#         plot.background = element_rect(fill = "lightblue"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         axis.line = element_line(size = .6, colour = "black")) +
#   ylim(300,600) +
#   ylab("PISA reading score") + xlab("Total spending, age 6-15 ($)") + 
#   scale_x_continuous(label=scales::comma)
# 
# flag
######################


########
## Log transformation of X
########

# Piecewise regression estimates
lin +
  geom_smooth(method='lm', data=subset(pisa, total_spending<50000), se=F, color='black') +
  geom_smooth(method='lm', data=subset(pisa, total_spending>=50000), se=F, color='black')

# Exclude Qatar and compare
lin + 
  geom_smooth(method='lm', data=subset(pisa, total_spending<50000), se=F, color='black') +
  geom_smooth(method='lm', data=subset(pisa, total_spending>=50000), se=F, color='black', linetype='dotted') +
  geom_smooth(method='lm', data=subset(pisa, total_spending>=50000 & Country!="Qatar"), se=F, color='black')

# With confidence intervals
lin +
  geom_smooth(method='lm', data=subset(pisa, total_spending<50000), color='black') +
  geom_smooth(method='lm', data=subset(pisa, total_spending>=50000), color='black', linetype='dotted') +
  geom_smooth(method='lm', data=subset(pisa, total_spending>=50000 & Country!="Qatar"), color='black')

# The problem of overfitting
lin +
    geom_smooth(se=F, span=.1)

# Log_10 scale transformation
lin +
  xlab("Total spending, age 6-15 (Log10 $)") +
  scale_x_log10(breaks=c(10000, 50000, 100000, 300000), 
                label=scales::comma) +
  geom_smooth(method='lm')

# Regress outcome on log-transformed x
summary(lm(read_score ~ log10(total_spending), data=pisa))


######################
### Log transformations in Y
######################

# Start without Luxembourg and Ireland
oecd <- readxl::read_excel(here("data/oecd_2022.xlsx")) %>% filter(country!="Luxembourg" & country!="Ireland")

ppe <- ggplot(oecd, aes(x=gdp, y=ppe)) + 
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(size = .6, colour = "black")) +
  ylab("Total expenditure on primary to tertiary ed per student") + xlab("GDP per capita") + 
  ylim(2000, 23000) +
  scale_x_continuous(label=scales::comma)

ppe

# Manually create log2 transformation
oecd$log2ppe <- log2(oecd$ppe)

ggplot(oecd, aes(x=gdp, y=log2ppe)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(size = .6, colour = "black")) +
  ylab("Log2 total expenditure per student") + xlab("GDP per capita") + 
  scale_x_continuous(label=scales::comma) +
  ylim(12, 15) +
  geom_smooth(method='lm')


# Regress log2 transformed outcome on X
summary(lm(log2(ppe) ~ gdp, oecd))


######################
### Log-log transformations (both X and Y)
######################

# Bring the OECD data in with all the countries included
oecd2 <- readxl::read_excel(here("data/oecd_2022.xlsx"))


ggplot(oecd2, aes(x=gdp, y=ppe)) + 
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(size = .6, colour = "black")) +
  ylab("Total expenditure on primary to tertiary ed per student") + xlab("GDP per capita") + 
  scale_x_continuous(label=scales::comma)

# Log transform the data -- note that the default log command is the natural log
oecd2$lngdp <- log(oecd2$gdp)
oecd2$lnppe <- log(oecd2$ppe)

ggplot(oecd2, aes(x=lngdp, y=lnppe)) +
  geom_point() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(size = .6, colour = "black")) +
  ylab("Ln(Expenditure per student)") + xlab("Ln(GDP per capita)") + 
  geom_smooth(method='lm')


# Regress log-transformed y on log-transformed x
summary(lm(log(ppe) ~ log(gdp), oecd2))


##################################
## Polynomials
###################################

# Graphing a quadratic
flag + 
  geom_smooth(method='lm', formula = y ~ x + poly(x, 2))

# Estimating a quadratic 
summary(lm(read_score ~ poly(total_spending, 2), pisa))

#### Graphing a cubic
# Bring in the DIBELS data
dibels <- read.csv(here("data/dibels.csv"))


ggplot(dibels, aes(y1_boy_mean, y2_moy_mean)) +
  theme_minimal(base_size = 16) +
  geom_point(alpha=0.1) +
  geom_smooth(method='lm', formula = y ~ poly(x, 3))
