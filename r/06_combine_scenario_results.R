## Name: 06_combine_scenario_results.R
## Description: Present performance measures by R distribution 
## Input file: results tables
## Functions: 
## Output file: scenario_measures.qs

# Packages ----------------------------------------------------------------
library(dplyr)
library(flextable)
library(gtsummary)
library(officer)
library(tidyr)


# Set scenario reference ---------------------------------------------


scenario_path <- file.path("scenarios")
data_path <- file.path("data", "processing")
outputs_path <- file.path("outputs")


# Table 3 -------------------------------------------------------------
all_outputs <- list.files(outputs_path, recursive = T, full.names = T)
table_3_files <- grep("Table3\\.csv", all_outputs, value = T)

read_and_label <- function(csv_filename) {
  rlabel <- gsub(".*r\\_mean\\_|\\/Table3\\.csv", "", csv_filename)
  rlabel <- gsub("\\_", "\\.", rlabel)
  df <- read.csv(csv_filename, )
  df$r <- rlabel
  return(df)
}

table_3_list <- lapply(table_3_files, read_and_label)

table_3_scenarios <- dplyr::bind_rows(table_3_list)
table_3_scenarios$X <- NULL
names(table_3_scenarios) <- gsub("X|\\.$", "", names(table_3_scenarios))
names(table_3_scenarios) <- gsub("\\.", "-", names(table_3_scenarios))
dput(names(table_3_scenarios))

long <- table_3_scenarios %>%
  tidyr::pivot_longer(cols = c("10-99", "100-499", "500-999", "1000"),
                      names_to = "Population")

wide <- long %>%
  tidyr::pivot_wider(names_from = "r")

widem <- long %>%
  tidyr::pivot_wider(names_from = "measure")

wide_rm <- long %>%
  tidyr::pivot_wider(names_from = c("r", "measure"))

col_order <- c("true_value", "Population", 
               sort(grep("bias", names(wide_rm), value = T)),
               sort(grep("coverage", names(wide_rm), value = T)),
               sort(grep("empirical se", names(wide_rm), value = T)),
               sort(grep("model se", names(wide_rm), value = T)))


wide_rm <- wide_rm %>% select(eval(col_order))
r_means <- unique(table_3_scenarios$r)
r_means_n <- length(r_means)

brdr <- officer::fp_border(color = "darkgrey", width = 1)
fltbl <- wide_rm %>%
  flextable::flextable() %>%
  flextable::delete_part(part = "header") %>%
  flextable::add_header_row(
    values = c("Under reporting", "Population", "Bias", "Coverage", 
               "Empirical SE", "Model SE"), 
    colwidths = c(1, 1, rep(r_means_n, 4)), 
    top = T) %>%
  flextable::add_header_row(
    values = c("", "R mean", rep(r_means, 4)), top = F) %>%
  flextable::theme_booktabs() %>%
  flextable::vline(
    j=c(2,seq(2 + r_means_n, length(col_order) - r_means_n, r_means_n)),
    border = brdr) %>%
  flextable::fontsize(size = 7) %>%
  flextable::fontsize(size = 7.5, part = "header") %>%
  flextable::align(align = "center", part = "all")
fltbl

scenarios_dir <- file.path("outputs", "scenarios")
flextable::save_as_docx(
  fltbl, path = file.path(scenarios_dir, "table3_scenarios.docx"))



# Table 4


scenario_path <- file.path("scenarios")
data_path <- file.path("data", "processing")
outputs_path <- file.path("outputs")



all_outputs <- list.files(outputs_path, recursive = T, full.names = T)
table_4_files <- grep("Table4\\.csv", all_outputs, value = T)

read_and_label <- function(csv_filename) {
  rlabel <- gsub(".*r\\_mean\\_|\\/Table4\\.csv", "", csv_filename)
  rlabel <- gsub("\\_", "\\.", rlabel)
  df <- read.csv(csv_filename, )
  df$r <- rlabel
  return(df)
}

table_4_list <- lapply(table_4_files, read_and_label)

table_4_scenarios <- dplyr::bind_rows(table_4_list)
table_4_scenarios$X <- NULL
names(table_4_scenarios) <- gsub("X|\\.$", "", names(table_4_scenarios))
names(table_4_scenarios) <- gsub("\\.", "-", names(table_4_scenarios))
dput(names(table_4_scenarios))

long <- table_4_scenarios %>%
  tidyr::pivot_longer(cols = c("est_05", "est_10", "est_15", "est_20"),
                      names_to = "est")
wide_rm <- long %>%
  tidyr::pivot_wider(names_from = c("est", "r"))

col_order <- c("true_value", "reported_outbreak_bins", 
               sort(grep("est_05", names(wide_rm), value = T)),
               sort(grep("est_10", names(wide_rm), value = T)),
               sort(grep("est_15", names(wide_rm), value = T)),
               sort(grep("est_20", names(wide_rm), value = T)))
wide_rm <- wide_rm %>% select(eval(col_order))

est_labels <- c("≤ 5%", "≤ 10%", "≤ 15%", "≤ 20%")
r_means <- unique(table_3_scenarios$r)
r_means_n <- length(r_means)


brdr <- officer::fp_border(color = "darkgrey", width = 1)
fltbl <- wide_rm %>%
  arrange(true_value) %>%
  flextable::flextable() %>%
  flextable::delete_part(part = "header") %>%
  flextable::add_header_row(
    values = c("Under reporting", "Outbreak size bins", est_labels), 
    colwidths = c(1, 1, rep(r_means_n, 4)), 
    top = T) %>%
  flextable::add_header_row(
    values = c("", "Absolute error from true value"),
    colwidths = c(2, r_means_n * 4)) %>%
  flextable::add_header_row(
    values = c("", "R mean", rep(r_means, 4)), top = F) %>%
  flextable::theme_booktabs() %>%
  flextable::vline(
    j=c(2,seq(2 + r_means_n, length(col_order) - r_means_n, r_means_n)),
    border = brdr) %>%
  flextable::fontsize(size = 7) %>%
  flextable::fontsize(size = 7.5, part = "header") %>%
  flextable::align(align = "center", part = "all")

fltbl

scenarios_dir <- file.path("outputs", "scenarios")
flextable::save_as_docx(
  fltbl, path = file.path(scenarios_dir, "table4_scenarios.docx"))
