## Name: 04_create_measures.R
## Description: Calculate the performance measures
## Input file: sim_results_combined.qs
## Functions: 
## Output file: pmeasures.qs

# Packages ----------------------------------------------------------------
library(dplyr)
## epitools
## binom


# Source user written scripts ---------------------------------------------


# Read data ---------------------------------------------------------------

simdf <- qs::qread("data/sim_results_combined.qs")


fct_lvls <-   c("10-99", "100-499", "500-999", "1000+")

simdf$reported_outbreak_bins <- factor(
  simdf$reported_outbreak_bins, fct_lvls)



# Create and store performance measures -----------------------------------

simdf <- simdf %>% 
  mutate(
    true_under_reporting = factor(true_value, 
                                  levels = c("0.25", "0.5", "0.75"),
                                  labels = c("0.25", "0.50", "0.75")),
    est_05diff = bias > 0.05,
    est_10diff = bias > 0.10,
    est_25diff = bias > 0.25,
    est_50diff = bias > 0.50,
    norm_ci_high = estimated_reporting_probability + (1.96*se_reporting_probability),
    norm_ci_low = estimated_reporting_probability - (1.96*se_reporting_probability),
    cover_norm = norm_ci_high > true_value & norm_ci_low < true_value,
    t_dist = qt(0.975, df = n_known_epilink),
    t_ci_high = estimated_reporting_probability + (t_dist*se_reporting_probability),
    t_ci_low = estimated_reporting_probability - (t_dist*se_reporting_probability),
    wilson_ci_high = epitools::binom.wilson(n_known_epilink,n_reported)$upper,
    wilson_ci_low = epitools::binom.wilson(n_known_epilink, n_reported)$lower,
    clogclog_ci_high = binom::binom.cloglog(n_known_epilink, n_reported)$upper,
    clogclog_ci_low  = binom::binom.cloglog(n_known_epilink, n_reported)$lower,
    asymp_ci_high = binom::binom.asymp(n_known_epilink, n_reported)$upper,
    asymp_ci_low  = binom::binom.asymp(n_known_epilink, n_reported)$lower,
    probit_ci_high = binom::binom.probit(n_known_epilink, n_reported)$upper,
    probit_ci_low  = binom::binom.probit(n_known_epilink, n_reported)$lower,
    ac_ci_high = binom::binom.agresti.coull(n_known_epilink, n_reported)$upper,
    ac_ci_low  = binom::binom.agresti.coull(n_known_epilink, n_reported)$lower,
    lrt_ci_high = binom::binom.lrt(n_known_epilink, n_reported)$upper,
    lrt_ci_low  = binom::binom.lrt(n_known_epilink, n_reported)$lower,
    cover_norm = norm_ci_high > true_value & norm_ci_low < true_value,
    cover_t = t_ci_high > true_value & t_ci_low < true_value,
    cover_wilson = wilson_ci_high > true_value & wilson_ci_low < true_value,
    cover_clogclog = clogclog_ci_high > true_value & clogclog_ci_low < true_value,
    cover_asymp = asymp_ci_high > true_value & asymp_ci_low < true_value,
    cover_probit = probit_ci_high > true_value & probit_ci_low < true_value,
    cover_ac = ac_ci_high > true_value & ac_ci_low < true_value,
    cover_lrt = lrt_ci_high > true_value & lrt_ci_low < true_value,
  )


# Performance measures

pmeasures <- simdf %>% 
  group_by(true_value, reported_outbreak_bins) %>% 
  summarise(
    simulations = n(),
    sum_bias = sum(bias),
    sum_bias_sq = sum(bias_sq),
    sum_spread = sum(spread),
    sum_spread_sq = sum(spread_sq),
    sum_mse_num = sum(mse_num),
    # Performance measures
    average_point_estimate = mean(estimated_reporting_probability),
    median_point_estimate = median(estimated_reporting_probability),
    average_var = mean(se_reporting_probability^2),
    median_var = median(se_reporting_probability^2),
    average_se = sqrt(average_var),
    average_point_estimate_1se_plus  = average_point_estimate + average_se,
    average_point_estimate_1se_minus = average_point_estimate - average_se,
    # Bias
    bias_mean = mean(bias),
    bias_median = median(bias),
    bias_mcse = sd(bias),
    # Precision
    mod_se_bias = sqrt(mean(se_reporting_probability^2)),
    mod_se_bias_mcse = sqrt(var(se_reporting_probability^2)/
                              ((4*simulations)*mod_se_bias^2)),
    emp_se = sd(estimated_reporting_probability),
    emp_se_mcse = emp_se/(sqrt(2*(simulations-1))),
    mse_mcse = sqrt(sum_mse_num/(simulations*(simulations-1))),
    mod_se_err =  mod_se_bias/ emp_se,
    rmse = sqrt(mean(bias_sq)),
    rmse_mcse = sqrt(sqrt(sum_mse_num/(simulations*(simulations-1)))),
    # Coverage
    coverage = mean(covered),
    coverage_norm = mean(cover_norm),
    coverage_t = mean(cover_t),
    coverage_wilson = mean(cover_wilson),
    coverage_wilson = mean(cover_clogclog),
    coverage_asymp = mean(cover_asymp),
    coverage_probit = mean(cover_probit),
    coverage_ac = mean(cover_ac),
    coverage_lrt = mean(cover_lrt),
    coverage_mcse = sqrt((coverage * (1 - coverage))/n())
    
  ) 



# Number of simulations ---------------------------------------------------


simdf %>% 
  count(true_value, reported_outbreak_bins) 

all <- list(simdf, pmeasures)

qs::qsave(all, "data/pmeasures.qs")
☺

