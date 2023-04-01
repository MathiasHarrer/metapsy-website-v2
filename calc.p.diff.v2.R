# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                             #
#   FUNCTION TO CALCULATE PROPORTION DIFFERENCES FROM CENSUS DATA             #
#   (Based on the One-Sample Proportion Test)                                 #
#                                                                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Reference:
# Zou, K. H., Fielding, J. R., Silverman, S. G., & Tempany, C. M. (2003). 
# Hypothesis testing I: proportions. Radiology, 226(3), 609-6

# Function arguments:
# - prop.study: proportion in the study sample (value from 0 to 1)
# - n.study: sample size of the study
# - prop.study: proportion in the reference population (value from 0 to 1)


calc.p.diff = function(prop.study, n.study, 
                       prop.population) {
  
  
  if (missing(prop.study)){
    stop("Argument 'prop.study' is missing.")
  }
  if (missing(n.study)){
    stop("Argument 'n.study' is missing.")
  }
  if (missing(prop.population)){
    stop("Argument 'prop.population' is missing.")
  }
  
  p <- prop.study
  pi_0 <- prop.population
  
  rd <- p - pi_0
  se.rd <- sqrt((pi_0*(1-pi_0))/n.study)
  
  if (sum(n.study*pi_0 < 5 | 
          n.study*(1-pi_0) < 5)>0) {
    warning(
      "At least one study has a very small sample size.",
      " Standard errors are calculated assuming of approximate",
      " normality, which may not hold when n*prop.population or",
      " n*(1-prop.population) < 5. Results are marked with with",
      " small_sample = TRUE.")
    
    small.sample <- n.study*pi_0<5|n.study*(1-pi_0)<5
    
    return(data.frame(p_diff = rd, p_diff_se = se.rd, 
                      small_sample = small.sample))
  } else {
    return(data.frame(p_diff = rd, p_diff_se = se.rd))
  }
}



# Usage Examples --------------------------------------------------------------

# Create an example dataset 
data(state)
df <- data.frame(state = sample(state.abb, 20, TRUE), 
                 prop.population = sample(20:60/100, 20, TRUE),
                 prop.study = sample(20:60/100, 20, TRUE),
                 n.study = round(rweibull(20, 2, 100)))

# Calculate proportion difference values and standard errors
(calc.p.diff(prop.study = df$prop.study, 
             n.study = df$n.study, 
             prop.population = df$prop.population) -> es)

# Meta-analyze
library(meta)
dat <- cbind(df, es)
metagen(p_diff, p_diff_se, studlab = state, data = dat, 
        sm = "RD", hakn = TRUE,common = FALSE) |>
  forest()


# Calculate assuming the same population proportion
calc.p.diff(prop.study = df$prop.study, 
            n.study = df$n.study, 
            prop.population = 0.45)

# The formula used here assume approximate normality, which
# is not met when the case when both the sample size and 
# population-level proportion are very small. A warning is 
# printed when this is the case.
calc.p.diff(prop.study = c(0.3, 0.4, 0.4), 
            n.study = c(100, 85, 50), 
            prop.population = c(0.23, 0.01, 0.3))



# ADDENDUM FOR NINO:
# It is also possible to prepare the data a little further.
# Then, metapsyTools can also be used for the analysis.

library(dplyr)
library(metapsyTools) # 1.0.9 [BETA]

# I'll prepare the example data set dat (see above) in a way that
# metapsyTools can use. Of course it's probably easiest in practice to do that
# using Excel,
within(dat, {
  study = LETTERS[1:nrow(dat)]
  minority = sample(c("Hispanic", "Black", "Asian/PI"), nrow(dat), rep=T)
  rob = sample(1:4, nrow(dat), rep=T)}) %>% 
  rename(.g = p_diff, .g_se = p_diff_se) %>% 
  select(study, state, minority, rob, .g, .g_se) -> dat

# Have a look at dat now. It has the following required columns:
# - study
# - minority
# - rob rating (numeric)
# - .g (which used to be p_diff)
# - .g_se
# Of course you can add more columns than that, for example for subgroup analyses
dat

# Then, we only select the minority in question and runMetaAnalysis.
dat %>% 
  filterPoolingData(minority == "Black") %>% 
  runMetaAnalysis() %>% 
  plot("combined")




