## Name: 05_analyse_sims.R
## Description: Analyse results of the simulation studies
## Input file: sim_results
## Functions: 
## Output file: sim_results_combined.qs

# Packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemr)

# library(binom)
# library(tidyr)


# Source user written scripts ---------------------------------------------

ggthemr("fresh")

true_val_col <- c("#153B6E", "#4D5E61", "#8E7E65")

all <- qs::qread('data/pmeasures.qs')

simdf <- all[[1]]
pmeasures <- all[[2]]


# Figure one --------------------------------------------------------------


fig1 <- ggplot(simdf) +
  scale_colour_manual(values = true_val_col) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) + 
  geom_jitter(aes(x = estimated_reporting_probability,
                  y = reported_outbreak_bins,
                  col = true_under_reporting),
              alpha = 0.06, height = 0.10) +
  # geom_linerangeh(data = pmeasures, 
  #            aes(xmin = average_point_estimate_1se_minus,
  #                xmax = average_point_estimate_1se_plus,
  #                y = reported_outbreak_bins, group = true_value),
  #                col = "black", size = 0.5) +
  ylab("Reported outbreak size") +
  geom_point(data = pmeasures, 
             aes(x = average_point_estimate, 
                 y = reported_outbreak_bins),
             col = "darkred", shape = "|", size = 10) +
  ylab("Reported outbreak size") +
  xlab("Estimated reporting") +
  guides(colour = guide_legend(title = "True reporting",
                               override.aes = list(alpha = 1, size = 5),
                               byrow = TRUE,
                               direction = "horizontal",
                               title.position = "left",
                               label.position = "left", keywidth = 0.1,
  )) +
  theme(legend.text= element_text( size = 14),
        legend.position = c(0.29, 0.96),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.1, "cm"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        text = element_text( size = 14)
  )

ggsave(fig1, filename = "outputs/figure_1.png", height = 10, width = 8)


# Zip plots ---------------------------------------------------------------

zip_data <- simdf %>% 
  mutate(diff = 
           abs(
             (true_value - estimated_reporting_probability)/
               se_reporting_probability)) %>% 
  arrange(reported_outbreak_bins, true_value, desc(covered), diff) %>% 
  group_by(reported_outbreak_bins, true_value) %>% 
  mutate(row_number = row_number(),
         true_value_zip = factor(true_value, 
                                 levels = c(0.25, 0.5, 0.75),
                                 labels = c("True reporting\n\n0.25", "0.50", "0.75")),
         covered = factor(covered, 
                          levels = c("TRUE", "FALSE"),
                          labels = c("covered", "not-covered"))
  )


fig2 <- zip_data %>%  
  ggplot(aes(y = row_number, x = reporting_conf_low, col = covered)) +
  geom_segment(aes(yend = row_number, xend = reporting_conf_high)) +
  facet_grid(true_value_zip ~ reported_outbreak_bins, ) +
  scale_y_continuous(labels = function(x) x/2000, 
                     breaks = c(0.05*2000, 0.5*2000, 0.95*2000)) + 
  scale_x_continuous(breaks = c(0.25, 0.50, 0.75)) + 
  geom_vline(aes(xintercept = true_value), col = "white") + 
  labs(subtitle = "Reported outbreak size") +
  ylab(expression(paste("Fractional centile of |z| for z = ", (pi[i] - pi)/SE[i]))) +
  xlab("Proportion of reporting") +
  scale_color_manual(values = c( "#6B9D92", "#8F9095"),
                     labels = c( "Coverer", "Non-coverers")) +
  guides(colour = guide_legend(
    override.aes = list(size = 3),
    byrow = TRUE,
    direction = "horizontal",
    title.position = "left",
    label.position = "right")) +
  theme(legend.text= element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0, "cm"),
        text = element_text( size = 14),
        axis.text.x.bottom = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "grey"),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(0.5, "lines"),
        strip.text.y = element_text(angle = 0, vjust = 0.75, hjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = -0.1)
  )

ggsave(fig2, filename = "outputs/figure_2.png", height = 10, width = 8)


# Performance measures tables ---------------------------------------------


options(scipen=999)
rounder <- function(a , b) {
  paste0(round(a, 4), " (", round(b, 4), ")")
}
cov_rounder <- function(a , b) {
  paste0(round(a, 4)*100, "% (", round(b, 4)*100, ")")
}

compare <- function(a, b){
  100*((a^2/b^2)-1)
}

pmeasures  %>% 
  select(reported_outbreak_bins, true_value, emp_se) %>% 
  pivot_longer(cols = 
                 -one_of(c("true_value", "reported_outbreak_bins")),
               names_to = "measure") %>% 
  pivot_wider(names_from = reported_outbreak_bins,
              values_from = value,
              names_prefix = "pop") %>% 
  mutate(
    relpop10_99 = compare(`pop10-99`, `pop10-99`),
    relpop100_499 = compare(`pop100-499`, `pop100-499`),
    relpop500_999 = compare(`pop100-499`, `pop500-999`),
    relpop1000 = compare(`pop100-499`, `pop1000+`)
  ) %>% 
  select(contains("rel"))

pmeasures  %>% 
  select(reported_outbreak_bins, true_value, emp_se) %>% 
  pivot_longer(cols = 
                 -one_of(c("true_value", "reported_outbreak_bins")),
               names_to = "measure") %>% 
  pivot_wider(names_from = true_value,
              values_from = value,
              names_prefix = "val") %>% 
  mutate(
    relval025 = compare(val0.25, val0.25),
    relval050 = compare(val0.25, val0.5),
    relval075 = compare(val0.25, val0.75),
  ) %>% 
  select(contains("rel"))

measures_df <- pmeasures %>% 
  mutate(bias = rounder(bias_mean, bias_mcse), 
         coverage = cov_rounder(coverage, coverage_mcse),
         coverage_norm = cov_rounder(coverage_norm, coverage_mcse),
         coverage_t = cov_rounder(coverage_t, coverage_mcse),
         coverage_wilson = cov_rounder(coverage_wilson, coverage_mcse),
         coverage_asymp = cov_rounder(coverage_asymp, coverage_mcse),
         coverage_probit = cov_rounder(coverage_probit, coverage_mcse),
         coverage_ac = cov_rounder(coverage_ac, coverage_mcse),
         coverage_lrt = cov_rounder(coverage_lrt, coverage_mcse),
         empirical_se = rounder(emp_se, emp_se_mcse),
         model_se = rounder(mod_se_bias, mod_se_bias_mcse),
         model_ratio = rounder(mod_se_err, 0),
         root_mean_squared_error = rounder(rmse, rmse_mcse)
  ) %>% 
  select(true_value, 
         reported_outbreak_bins,
         bias,
         coverage,
         coverage_norm,
         coverage_t,
         coverage_wilson,
         coverage_asymp,
         coverage_probit,
         coverage_ac,
         coverage_lrt,
         root_mean_squared_error,
         empirical_se,
         model_se,
         model_ratio
  )


measures_table <- measures_df %>% 
  pivot_longer(cols = 
                 -one_of(c("true_value", "reported_outbreak_bins")),
               names_to = "measure") %>% 
  pivot_wider(names_from = reported_outbreak_bins, values_from = value) %>% 
  select(measure, true_value, everything()) %>% 
  arrange(measure) 

measures_table$measure <- gsub("_", " ",
                               paste0(measures_table$measure))


measures_table


# Operational table -------------------------------------------------------



sumprop <- function(a){
  if(sum(a)>0){
    paste0(sum(a), " (", round(sum(a)/n()*100,2),"%)" )
  }else{
    "0"
  }
}


simdf %>% 
  group_by(true_value, reported_outbreak_bins) %>% 
  summarise(est_15 =    sumprop(est_15diff),
            est_20 =    sumprop(est_20diff),
            est_ci_15 = sumprop(est_ci_15diff),
            est_ci_20 = sumprop(est_ci_20diff)
  ) 
# kable(col.names = c("True reporting",
#                     "Reported outbreak size",
#                     "15% different",
#                     "20% different",
#                     "15% different",
#                     "20% different")) %>% 
# kable_styling(bootstrap_options = c("striped", "hover")) %>% 
# collapse_rows(columns = 1, valign = "middle") %>% 
# add_header_above(c("","",
#                    "Estimate" = 2, 
#                    "Confidence interval" = 2)) %>% 
# add_header_above(c("","",
#                    "Absolute difference from true value" = 4))

simdf %>% 
  group_by(reported_outbreak_bins) %>% 
  summarise(est_15 =    sumprop(est_15diff),
            est_20 =    sumprop(est_20diff),
            est_ci_15 = sumprop(est_ci_15diff),
            est_ci_20 = sumprop(est_ci_20diff)
  )

simdf %>% 
  group_by(true_value) %>% 
  summarise(est_15 =    sumprop(est_15diff),
            est_20 =    sumprop(est_20diff),
            est_ci_15 = sumprop(est_ci_15diff),
            est_ci_20 = sumprop(est_ci_20diff)
  )

