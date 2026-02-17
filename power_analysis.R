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
# library(mediation)
library(multilevelmediation)
# library(boot)


results_power <- data.frame(
  iteration         = numeric(),
  medi_eff_est      = numeric(),
  medi_eff_ci_lower = numeric(),
  medi_eff_ci_upper = numeric()
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
  # multilevelmediation package
  # ALTERNATIVE IS mlma package: https://cran.r-project.org/web/packages/mlma/vignettes/MLMAvignette.html
  boot_result <- boot.modmed.mlm.custom(data, 
                         nrep = 10,    # increase to 1000
                         L2ID = "class_id", 
                         X = "X",
                         Y = "Y",
                         M = "M",
                         seed=1234)
  
  # get indirect effect
  boot_result_ci <- extract.boot.modmed.mlm(boot_result, type="indirect", ci.conf=.95)
  
  
  results_power <- results_power %>%
    add_row(iteration    = i,
            medi_eff_est = boot_result_ci$est,
            medi_eff_ci_lower   = boot_result_ci$CI["2.5%"],
            medi_eff_ci_upper   = boot_result_ci$CI["97.5%"])
}

# Power for mediation (indirect) effect
results_power %>%
  mutate(sign = case_when(
                  medi_eff_ci_lower > 0 & medi_eff_ci_upper > 0 ~ 1,
                  medi_eff_ci_lower < 0 & medi_eff_ci_upper < 0 ~ 1,
                  TRUE ~ 0)) %>%
  summarize(power = mean(sign))



