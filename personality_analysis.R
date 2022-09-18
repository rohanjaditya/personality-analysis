### CS555 TERM PROJECT - FALL 2021
### ROHAN J ADITYA (U17792248)

### CUSTOMER PERSONALITY ANALYSIS

# ANALYZING WHETHER THERE IS A DIFFERENCE IN THE MEAN AMOUNT SPENT ON WINE BY
# PEOPLE GROUPED BY LEVEL OF EDUCATION
# THE FOUR GROUPS ARE:
#   2ND CYCLE
#   GRADUATION
#   MASTER
#   PHD

# In this project, I analyze whether there is a difference in the amount spent 
# on wine by people grouped by level of education. If the global F test is 
# significant, I also examine which of the pairwise group means are different. 
# I also test whether the population means of the amount spent on wines across 
# people grouped by education level is equal after controlling for income.

library(dplyr)
library(plotly)
library(car)
library(emmeans)


## IMPORT THE CSV FILE
data = read.csv("marketing_campaign.csv", sep="\t")


## PREPROCESSING
# 1. Remove rows which have a null value
data = na.omit(data)

# 2. Remove observations of which Education falls into 'Basic' because of
#    lack of data
data = data[!(data$Education=="Basic"),]

# 3. Select the columns required for analysis
data = data[,c("Education",
               "MntWines",
               "Income")]


## EXPLORATORY DATA ANALYSIS
# 1. Number of observations in each group
count(data, Education)

# 2. Summary statistics of each group
aggregate(data$MntWines, by=list(data$Education), summary)
aggregate(data$MntWines, by=list(data$Education), sd)

# 3. Histogram, Density and Box plots
summary_plots = function(df, title_name)
{
  hist = plot_ly(data = df,
                 x = ~MntWines,
                 type = "histogram",
                 name = "Histogram",
                 marker = list(color="#6eaed9",
                               line = list(color="#348aaa",
                                           width=2))) %>%
    layout(xaxis = list(title = "Amount Spent on Wine",
                        showgrid=T),
           yaxis = list(title = "Count",
                        showgrid=T),
           margin = list(t = 50,
                         b = 25))

  x = seq(min(df$MntWines), max(df$MntWines), length=100)
  y = dnorm(x, mean=mean(df$MntWines), sd=sd(df$MntWines))
  df_dens = data.frame(x,y)
  
  dens = plot_ly(data = df_dens,
                 x = ~x,
                 y = ~y,
                 type = "scatter",
                 mode = "lines",
                 name = "Density",
                 line = list(color = "red")) %>%
    
    layout(xaxis = list(title = "Amount Spent on Wine",
                        showgrid=T),
           yaxis = list(title = "Density",
                        showgrid=T),
           margin = list(t = 50,
                         b = 25))
  
  splot1 = subplot(hist, 
                   dens, 
                   nrows = 2,
                   shareX = TRUE,
                   titleY = TRUE,
                   titleX = TRUE,
                   margin = 0.075)
  
  splot2 = plot_ly(data = df,
                   y = ~MntWines,
                   type = 'box',
                   name = "Box Plot") %>%
    
    layout(yaxis = list(title = "Amount Spent on Wine",
                        showgrid=T),
           margin = list(t = 50,
                         b = 25))
  
  subplot(splot1,
          splot2,
          titleX = TRUE,
          titleY = TRUE,
          margin = 0.075) %>%
    
    layout(title = paste("Amount Spent on Wine by Group:", 
                         title_name))
}


summary_plots(subset(data, Education=='2n Cycle'), "2nd Cycle")
summary_plots(subset(data, Education=='Graduation'), "Graduation")
summary_plots(subset(data, Education=='Master'), "Master ")
summary_plots(subset(data, Education=='PhD'), "PhD")


## ONE-WAY ANOVA ASSUMPTIONS

# 1. Independence of samples from each group
# There is no evidence to suggest that there is a relationship between the 
# observations in each group. Therefore, we can say that the groups are 
# independent.

# 2. Normal distribution of response variable
summary_plots(data, "Overall")
# From the summary plots, it can be seen that the distribution of the 
# amount spent on wine by all the groups does not follow a perfectly normal
# distribution. The distribution is skewed to the right.
# This assumption is not met, but since the skewness is not extreme, the
# analysis is taken forward.

# 3. Population variance of response variable
# Population variances of the amount spent on wine
aggregate(data$MntWines, by=list(data$Education), var)

# Largest value divided by smallest value
paste("Largest sample variance divided by smallest sample variance:",
      format(max(aggregate(data$MntWines, 
                           by=list(data$Education), 
                           var)$x) / 
               min(aggregate(data$MntWines, 
                             by=list(data$Education), 
                             var)$x),
             digits=3))
# The largest sample variance divided by the smallest sample variance is
# greater than 2, but not by a large margin.
# This assumption is not met, but since the ratio is close to 2, the analysis
# is taken forward.


## POWER ANALYSIS

# 1. Power Curve
b.var = var(aggregate(data$MntWines, by=list(data$Education), mean)$x)

w.var = mean(aggregate(data$MntWines, by=list(data$Education), var)$x)

n = seq(2,100, by=2)

p = power.anova.test(groups=5,
                     between.var = b.var,
                     within.var = w.var, 
                     sig.level=0.05,
                     n=n,
                     power=NULL)

plot_ly(x=n,
        y=p$power,
        type='scatter',
        mode='markers') %>%
  layout(title = "Power Curve",
         xaxis = list(title = "Sample Size"),
         yaxis = list(title = "Power"),
         margin = list(t = 50,
                       b = 25))

# 2. Number of observations in each group
count(data, Education)

# Therefore, the number of samples is sufficient


## ONE-WAY ANOVA MODEL

# Step 1
# H0 - The underlying population means for all four groups are equal
# H1 - Not all of the underlying population means are equal
# Significance level = 0.05

# Step 2
# The test statistic is the F statistic with k-1 and n-k degrees
# of freedom

# Number of groups:
k = length(unique(data$Education))

# Number of observations across all groups:
n = nrow(data)

paste("Degrees of freedom:",
      k-1,
      "and",
      n-k)
# Therefore the test statistic is the F statistic with 3 and 2158 
# degrees of freedom

# Step 3
# Critical F value:
paste("Critical F value for a significance level of 0.05 and",
      k-1,
      "and",
      n-k,
      "degrees of freedom:",
      format(qf(0.95, k-1, n-k), digits=7))

# Decision rule:
# Reject H0 if F>= 2.609026 or p value < 0.05
# Otherwise, do not reject H0

# Step 4
# The model
data$Education = factor(data$Education)

model = aov(MntWines~Education, data=data)

# Model Summary
summary(model)

# Step 5
# Since the F statistic is greater than the critical value, we reject the
# null hypothesis
# We have significant evidence at the significance level of 0.05 to say that
# there is a difference in the amount spent on wine by the four groups


## PAIRWISE COMPARISONS

# Step 1
# Null hypotheses:
#   The mean amount spent on wines is the same for each of the following pairs
#     Graduation - 2nd Cycle
#     Master - 2nd Cycle 
#     PhD - 2nd Cycle 
#     Master - Graduation
#     PhD  - Graduation
#     PhD - Master
# Alternate hypotheses:
#   The mean amount spent on wines is different for the following pairs
#     Graduation - 2nd Cycle
#     Master - 2nd Cycle 
#     PhD - 2nd Cycle 
#     Master - Graduation
#     PhD  - Graduation
#     PhD - Master

# Step 2
# The test statistic is the Studentized Range Distribution with 
# k = 4 and N-k = 2162-4 = 2158 degrees of freedom.

# Step 3
# Reject the null hypothesis if the adjusted p value is less than 
# the significance level.

# Step 4
TukeyHSD(model)

# Step 5
# Since the adjusted p value is less than the significance level for all
# the pairs except Master - Graduation, we reject the null hypothesis for all
# the pairs except Master - Graduation.
# We have sufficient evidence at the 0.05 significance level to say that the 
# mean amount spent on wines is different for all the pairs except 
# Master - Graduation
# In the case of Master - Graduation, we fail to reject the null hypothesis.
# We do not have sufficient evidence to say that the mean amount spent on wines
# by the groups Master and Graduation is different.


## ONE-WAY ANCOVA

# Adjusting for income
model_ancova = lm(MntWines~Education+Income, data=data)
Anova(model_ancova, type=3)
# It can be seen that education level is still significant with a p value of
# 4.305e-12.

# Least square means
emm_options(contrasts=c("contr.treatment", "contr.poly"))
emmeans(model_ancova, specs = "Education")
# Therefore, after controlling for income, the mean amount spent on wines by
# each of the groups is as follows
# 2nd cycle - 242
# Graduation - 287
# Master - 334
# PhD - 384
