# Measuring the unknown: an estimator and simulation study for assessing case reporting during epidemic
Repository for simulations of underreporting method, see preprint here: https://www.biorxiv.org/content/10.1101/2021.02.17.431606v1

## Folder structure:
* `r`: contains r scripts for running the simulations
* `r/functions`: contains user written functions used for simulations
* `outputs`: Tables and Figures in the paper
* `data`: simulated data
* `artwork`: figure explaining method used in the paper.

## Scripts
* `00_packages`: This can be run once to install the needed packages.
* `01_simulate_outbreaks.R`: Simulates outbreaks of various sizes.
* `02_remove_cases.R`: Removes a proporiton of cases from the simulated outbreaks.
* `03_combine_sims.R`: Combines all the simulations together.
* `04_create_measures.R`: Calculates the performance measures used to evaluate the method under the simulations.
* `05_analyse_sims.R`: Create graphs and analysis of the simulations using the performance measures. 
