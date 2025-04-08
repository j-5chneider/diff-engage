############################################################################## #
### SET VALUES HERE                                                         ####
############################################################################## #

a <-  0.278       # effect from X -> M
b <-  0.244       # effect from M -> Y
cp <- 0.146         # c prime, direct effect = c'

n_L2 <- 85        # 85 classes
n_L1 <- 25        # about 25 students per class


############################################################################## #
### SIMULATE DATA (leave this alone)                                        ####
############################################################################## #


library(dplyr)
library(lme4)
library(mediation)


results_power <- data.frame(
  iteration    = numeric(),
  medi_eff_est = numeric(),
  medi_eff_p   = numeric()
)

for(i in 1:10){
  set.seed(i) # for reproducibility
  
  # Cluster-Level DataFrame mit zufälligen Intercepts für M und Y
  class_df <- data.frame(
    class_id = 1:n_L2,
    X = rnorm(n_L2, mean = 0, sd = 1),
    u0_M = rnorm(n_L2, mean = 0, sd = 0.4),  # Random Intercepts für M
    u0_Y = rnorm(n_L2, mean = 0, sd = 0.4)   # Random Intercepts für Y
  )
  
  # Wiederhole auf Level-1
  data <- class_df %>%
    slice(rep(1:n_L2, each = n_L1)) %>%
    mutate(student_id = 1:(n_L1*n_L2))
  
  # Simuliere Mediator M (mit Random Intercept)
  data <- data %>%
    mutate(M = a * X + u0_M + rnorm(n(), sd = 1))
  
  # Simuliere Outcome Y (mit Random Intercept)
  data <- data %>%
    mutate(Y = cp * X + b * M + u0_Y + rnorm(n(), sd = 1))
  
  
  ############################################################################## #
  ### COMPUTE RESULTS (leave this alone)                                      ####
  ############################################################################## #
  
  
  ## Taken from
  # Tingley, D., Yamamoto, T., Hirose, K., Keele, L., & Imai, K. (2014). mediation: R Package for Causal Mediation   Analysis. Journal of Statistical Software, 59(5), 1–38. https://doi.org/10.18637/jss.v059.i05
  # https://stats.stackexchange.com/questions/491503/mediation-analysis-for-mixed-models
  
  fit.totaleffect <- lme4::lmer(Y ~ X + (1| class_id), data = data)
  fit.mediator    <- lme4::lmer(M ~ X + (1| class_id), data = data)
  fit.dv          <- lme4::lmer(Y ~ M + X + (1| class_id), data = data)
  
  results <- mediation::mediate(fit.mediator, fit.dv, treat='X', mediator='M')
  
  results_power <- results_power %>%
    add_row(iteration    = i,
            medi_eff_est = results$d.avg,
            medi_eff_p   = results$d.avg.p)
}

# Power for mediation (indirect) effect
results_power %>%
  mutate(sign = ifelse(medi_eff_p <= .05, 1, 0)) %>%
  summarize(power = mean(sign))
