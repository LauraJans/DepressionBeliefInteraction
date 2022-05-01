#####################################
##  Depression-Belief Interaction  ##
#####################################

####  Startup  ####
# Load packages

#install.packages("car")
#install.packages("olsrr")
#install.packages("corrplot")

library(tidyverse)
library(here)
library(ggplot2)
#library(sjPlot)
library(interactions)
library(car)
library(olsrr)
#library(corrplot)


#### Load data ####
df_1 <- readRDS(here("data", "T2T Data for Jans et al.rds"))



####  Prepare Data  ####
df_2 <- df_1 %>%
  
  # Adding a column for CDI mean
  mutate(cdi_mean = rowMeans(select(.,contains("cdi")), na.rm = FALSE)) %>%
  
  # Selecting variables
  select(yb_lsmh_id,
         # Demographic variables
         pb_childgender, pb_income, pb_childethnicity, pb_childage,
         # Variables for analysis
         yb_permanence, yb_cause_brain, yb_cause_env, yb_change_brain, yb_change_env, cdi_mean) %>%
  
  # Removing rows containing NA
  filter(complete.cases(.)) %>%
  
  # Standardize numeric variables
  mutate(across(c(yb_permanence, yb_cause_brain, yb_cause_env, yb_change_brain, yb_change_env, cdi_mean), 
                scale,
                .names = "{col}_scaled")) %>%
  
  # Change class to numeric
  mutate(across(c(yb_permanence_scaled, yb_cause_brain_scaled, yb_cause_env_scaled, yb_change_brain_scaled, 
                  yb_change_env_scaled, cdi_mean_scaled), as.numeric))
  
gender_table <- table(df_2$pb_childgender)
prop.table(gender_table)

race_eth_table <- table(df_2$pb_childethnicity)
prop.table(race_eth_table)

####  Analysis  ####

## Regression with CDI as covariate

my_model_1 <- lm(data = df_2, yb_permanence_scaled ~ yb_cause_brain_scaled + yb_change_brain_scaled + yb_cause_brain_scaled*yb_change_brain_scaled + cdi_mean_scaled)
summary(my_model_1)

my_model_2 <- lm(data = df_2, yb_permanence_scaled ~ yb_cause_env_scaled + yb_change_env_scaled + yb_cause_env_scaled*yb_change_env_scaled + cdi_mean_scaled)
summary(my_model_2)

## Regression without CDI

my_model_3 <- lm(data = df_2, yb_permanence_scaled ~ yb_cause_brain_scaled + yb_change_brain_scaled + yb_cause_brain_scaled*yb_change_brain_scaled)
summary(my_model_3)

my_model_4 <- lm(data = df_2, yb_permanence_scaled ~ yb_cause_env_scaled + yb_change_env_scaled + yb_cause_env_scaled*yb_change_env_scaled)
summary(my_model_4)

## Plots

interaction.plot(x.factor=df_2$yb_cause_brain, trace.factor=df_2$yb_change_brain, response=df_2$yb_permanence, 
                 xlab="Belief that depression is caused by the brain", 
                 ylab="Belief that depression is permanent", 
                 trace.label="Belief that brain can change")

interaction.plot(x.factor=df_2$yb_cause_env, trace.factor=df_2$yb_change_env, response=df_2$yb_permanence, 
                 xlab="Belief that depression is caused by the env", 
                 ylab="Belief that depression is permanent", 
                 trace.label="Belief that env can change")

## Plots grouping permanence variable into high and low malleability 
#(to create an interaction plot more similar to the pre-registration)

median_brain_malleability <- median(df_2$yb_change_brain)

median_env_malleability <- median(df_2$yb_change_env)

df_3 <- df_2 %>% mutate(malleability_brain = case_when(yb_change_brain <= median_brain_malleability ~ "low",
                                                       yb_change_brain > median_brain_malleability ~ "high"),
                        malleability_env = case_when(yb_change_env <= median_env_malleability ~ "low", 
                                                     yb_change_env > median_env_malleability ~ "high"))

interaction.plot(x.factor=df_3$yb_cause_brain, trace.factor=df_3$malleability_brain, response=df_3$yb_permanence)

interaction.plot(x.factor=df_3$yb_cause_env, trace.factor=df_3$malleability_env, response=df_3$yb_permanence)

## Generating histograms of each variable

ggplot(df_3, aes(yb_permanence)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression permanence belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_3$yb_permanence), sd = sd(df_3$yb_permanence)), col="red")

ggplot(df_3, aes(yb_cause_brain)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression caused by brain belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_3$yb_cause_brain), sd = sd(df_3$yb_cause_brain)), col="red")

ggplot(df_3, aes(yb_cause_env)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Depression caused by environment belief")+ 
  stat_function(fun=dnorm,args = list(mean = mean(df_3$yb_cause_env), sd = sd(df_3$yb_cause_env)), col="red")

ggplot(df_3, aes(yb_change_brain)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Brain can change belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_3$yb_change_brain), sd = sd(df_3$yb_change_brain)), col="red")

ggplot(df_3, aes(yb_change_env)) +  geom_histogram(aes(y=..density..), binwidth = 1) + ggtitle("Environment can change belief") + 
  stat_function(fun=dnorm,args = list(mean = mean(df_3$yb_change_env), sd = sd(df_3$yb_change_env)), col="red")

## Brain Regression Results

plot_model(my_model_1, type="pred", terms= "yb_cause_brain_scaled")
plot_model(my_model_1, type="pred", terms= "yb_change_brain_scaled")
interact_plot(my_model_1, pred= yb_cause_brain_scaled, modx= yb_change_brain_scaled, plot.points = FALSE, interval = TRUE, main.title = "Brain Interaction Controlling for CDI")

## Environment Regression Results

plot_model(my_model_2, type="pred", terms= "yb_cause_env_scaled")
plot_model(my_model_2, type="pred", terms= "yb_change_env_scaled")
interact_plot(my_model_2, pred= yb_cause_env_scaled, modx= yb_change_env_scaled, plot.points= FALSE, interval = TRUE, main.title = "Env Interaction Controlling for CDI")
interact_plot(my_model_4, pred= yb_cause_env_scaled, modx= yb_change_env_scaled, plot.points= FALSE, interval = TRUE, main.title = "Env Interaction Not Controlling for CDI")

#### Summary Statistics ####

count(df_3, pb_childgender) %>%
  mutate(pct = percent(n / sum(n)))

count(df_3, pb_childethnicity) %>%
  mutate(pct = percent(n / sum(n)))

count(df_3, pb_income) %>%
  mutate(pct = percent(n / sum(n)))


#### Checking Assumptions####

## Multicollinearity

corr_matrix <- df_3 %>%
  select(yb_cause_brain, yb_change_brain, yb_cause_env, yb_change_env, cdi_mean) %>%
  cor()

#print(corr_matrix)
corrplot(corr_matrix, method = 'number')

## Linearity of data; residuals should have a mean of zero

plot(my_model_1, 1)
plot(my_model_2, 1)

## Predictors are independent; p>.05 indicates that independence assumption is met

durbinWatsonTest(my_model_1)
durbinWatsonTest(my_model_2)

## Constant Variance of residual errors; p>.05 indicates that constant variance assumption is met

plot(my_model_1, 3)
plot(my_model_2, 3)

ncvTest(my_model_1)
ncvTest(my_model_2)

## Checking normality of residuals

ols_plot_resid_qq(my_model_1)
ols_plot_resid_qq(my_model_2)

ols_test_normality(my_model_1)
ols_test_normality(my_model_2)

