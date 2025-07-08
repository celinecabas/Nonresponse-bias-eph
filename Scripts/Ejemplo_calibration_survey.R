library(dplyr) #data manipulation
library(janitor)#Display marginals and totals 
#This library contains data on the total population of a hypothetical city.
library(TeachingSampling)#specialized bookstore to extract samples 
library(survey)#library to analyze surveys

# https://medium.com/@jon77mon/calibration-of-survey-weights-with-survey-in-r-54b2860a04c6

options(scipen = 999, digits = 4)

data("BigCity")  # We load the sample frame
glimpse(BigCity) # We explore the framework


# We calculate the proportion of expenditure
BigCity$x <- BigCity$Expenditure / sum(BigCity$Expenditure)

# We visualize the distribution of spending in the framework
hist(BigCity$x, col = "skyblue1", main = "Proportion of expenditure")

# We draw a sample proportional to the expenditure
smple <- BigCity %>%
  sample_frac(size = 9000 / nrow(BigCity), replace = FALSE, weight = x)

hist(smple$x, col = "skyblue2", main = "Proportion of expenditure (sample)")

tabyl(BigCity$Poverty) %>% adorn_totals()
tabyl(smple$Poverty) %>% adorn_totals()

# Calculation of totals by stratum in the frame
Nh <- BigCity %>%
  group_by(Stratum) %>%
  count() %>%
  rename(Nh = n)

# We incorporate the total by stratum in the sample and calculate the inverse weight of the sample proportion.
smple <- smple %>%
  left_join(Nh, by = "Stratum") %>%
  group_by(Stratum) %>%
  mutate(aux = 1, nh = sum(aux)) %>%
  ungroup() %>%
  select(-aux) %>%
  mutate(ph1 = nh / Nh,
         pw  = 1 / ph1)

# We define the sample design
dsg <- svydesign(ids = ~1,
                 strata = ~Stratum,
                 weights = ~pw,
                 data = smple)

# We define the calibration formula and the population totals.
formula <- ~Sex + Poverty 
T <- apply(model.matrix(formula, data = BigCity), 2, sum)

# Calibration with raking
raking <- calibrate(dsg, formula, population = T, calfun = "raking")
smple$pw_raking <- weights(raking)

# Linear calibration
linear <- calibrate(dsg, formula, population = T, calfun = "linear")
smple$pw_linear <- weights(linear)

# Calibración logit (útil para acotar pesos y evitar extremos)
logit <- calibrate(dsg, formula, population = T, calfun = "logit")
smple$pw_logit <- weights(logit)

# Sex tabulation in the framework
BigCity %>% 
group_by(Sex) %>% 
count() %>% 
adorn_totals()

#Tabulation of Sex before calibrated weights 
smple %>% 
group_by(Sex) %>% 
summarise(y = sum(pw)) %>% 
adorn_totals()

# Tabulation of Sex with calibrated weights (e.g., raking method)
smple %>% 
group_by(Sex) %>% 
summarise(y = sum(pw_raking)) %>% 
adorn_totals()
