# Heterogeneity Test For Stratified Models
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 02/17/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create single dataframe w all effect estimates
# 2: Create heterogeneity test function
# 3: Evaluate heterogeneity between strata


####**************
#### N: Notes #### 
####**************

# This function was originally written by Yanelli Nunez. I have 
# adapted it slightly for application in this analysis. 

# The goal of this script is to combine all stratified model results
# into one dataframe, and then to run the Wald test and Cochran test on
# pairs of strata to determine if they are statistically different from
# each other.

# The statistics are based on details in this article:
# Kaufman, J. S. and MacLehose R. F. 2013. Which of these things is not like  
# the others? Cancer. pp4216-4222. doi:10.1002/cncr.28359


####********************
#### 0: Preparation #### 
####********************

# 0a Load data
mod_hourly <- readRDS("outputs/mod_hourly.rds")
mod_weekend <- readRDS("outputs/mod_weekend.rds")
mod_roadside <- readRDS("outputs/mod_roadside.rds")


####*******************************************************
#### 1: Create single dataframe w all effect estimates #### 
####*******************************************************

# 1a Create empty df to hold Betas and SEs from all models
#    Note: We only need three variables - the name of 
#          the strata, the beta coefficient, and the standard error
model_estimates <- tibble(strata = NA, beta = NA, se = NA)

# 1b Pull intervention effects for roadside models
model_estimates <- model_estimates %>% 
  add_row(strata = "nonroadside",
          beta = summary(mod_roadside$mods[[1]])$tTable[2,1],
          se = summary(mod_roadside$mods[[1]])$tTable[2,2]) %>% 
  add_row(strata = "roadside",
          beta = summary(mod_roadside$mods[[2]])$tTable[2,1],
          se = summary(mod_roadside$mods[[2]])$tTable[2,2]) %>% 
  filter(!is.na(strata))

# 1c Pull intervention effects for weekend/weekday models
model_estimates <- model_estimates %>% 
  add_row(strata = "weekday",
          beta = summary(mod_weekend$mods[[1]])$tTable[2,1],
          se = summary(mod_weekend$mods[[1]])$tTable[2,2]) %>% 
  add_row(strata = "weekend",
          beta = summary(mod_weekend$mods[[2]])$tTable[2,1],
          se = summary(mod_weekend$mods[[2]])$tTable[2,2])

# 1d Pull intervention effects for hourly models
for(i in 1:length(mod_hourly$time_of_day)){
  model_estimates <- model_estimates %>% 
    add_row(strata = paste0("hour_", mod_hourly$time_of_day[[i]]),
            beta = summary(mod_hourly$mods[[i]])$tTable[2,1],
            se = summary(mod_hourly$mods[[i]])$tTable[2,2])
}


####*******************************************
#### 2: Create heterogeneity test function #### 
####*******************************************

# Begin function
het_test <- function(model_estimates, strata.1, strata.2) {
  # Example inputs
  #model_estimates = model_estimates
  #strata.1 = "hour_1"
  #strata.2 = "hour_6"
  
  # 2a Pull strata.1 from dataframe and create variance variable
  #    Notes: Here we could also exponentiate betas or convert
  #           confidence intervals to standard errors, depending
  #           on what type of model the estimates come from
  strata.1.df <- model_estimates %>%
    filter(strata == strata.1) %>%
    mutate(beta.1 = beta,
           se.1 = se,
           var.1 = se.1^2) %>%
    dplyr::select(beta.1, se.1, var.1)

  # 2b Pull strata.2 from dataframe and create variance variable
  strata.2.df <- model_estimates %>%
    filter(strata == strata.2) %>%
    mutate(beta.2 = beta,
           se.2 = se,
           var.2 = se.2^2) %>%
    dplyr::select(beta.2, se.2, var.2)
  
  # 2c Calculate heterogeneity test statistics and p-values
  # Note: Here we calculate both (1) the Wald test statistic and  
  #       a p-value based on z-score, and (2) the Cochran test
  #       statistic with a p-value based on Chi Square
  #       May need to adjust whether pnorm uses lower.tail
  #       or higher.tail; see commentary in ?pnorm
  test <- bind_cols(strata.1.df, strata.2.df) %>%
    mutate(diff.beta = beta.1 - beta.2,
           se.diff = sqrt((se.1)^2 + (se.2)^2),
           test.wald = diff.beta/se.diff,                       # wald test
           pnorm = pnorm(test.wald, lower.tail = F) * 2,        # p-value
           pooled.beta = ((beta.1/var.1) + (beta.2/var.2)) / 
             ((1/var.1) + (1/var.2)),
           test.cochran = ((beta.1 - pooled.beta)^2/var.1) + 
             ((beta.2 - pooled.beta)^2/var.2),                  # cochran test
           pchisq = 1 - pchisq(test.cochran, 1))                # p-value
  
  # 2d Print dataframe with test statistics
  test <- test %>% dplyr::select(test.wald, pnorm, test.cochran, pchisq)
  print(test)
  
}


####**********************************************
#### 3: Evaluate heterogeneity between strata #### 
####**********************************************

# 3a Run het_test: hour of day
#    Note: Both Wald and Cochran test significant
het_test(model_estimates = model_estimates,
         strata.1 = "hour_1",
         strata.2 = "hour_6")

# 3b Run het_test: weekend / weekday
#    Note: Both Wald and Cochran test NOT significant
het_test(model_estimates = model_estimates,
         strata.1 = "weekend",
         strata.2 = "weekday")

# 3c Run het_test: roadside / nonroadside
#    Note: Both Wald and Cochran test significant
het_test(model_estimates = model_estimates,
         strata.1 = "roadside",
         strata.2 = "nonroadside")


