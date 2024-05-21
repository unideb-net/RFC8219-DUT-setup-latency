# Loading the necessary packages
library(tidyverse)
library(broom)

# Folder paths
single_dut2_path <- "DUT2"
single_dut3_path <- "DUT3"
dual_dut_path <- "DUT2-DUT3"

# File names
files <- c("N1.csv", "N2.csv", "N4.csv", "N8.csv", "N16.csv", "N32.csv", "N64.csv", "N128.csv", "N256.csv")

# Reading data and calculating medians
read_and_calculate_median <- function(path, file) {
  df <- read_csv(file.path(path, file))
  median(df$Fwd_TL, na.rm = TRUE)
}

# Collecting data
results <- tibble(
  Network = as.numeric(c(1, 2, 4, 8, 16, 32, 64, 128, 256)),
  DUT2 = sapply(files, read_and_calculate_median, path = single_dut2_path),
  DUT3 = sapply(files, read_and_calculate_median, path = single_dut3_path),
  DUT2_DUT3 = sapply(files, read_and_calculate_median, path = dual_dut_path)
)

# Sum of latencies for Single DUT
results <- results %>%
  mutate(Single_DUT_Sum = DUT2 + DUT3)

# Analysis and auxiliary variables
results <- results %>%
  mutate(
    Condition_1_2_4 = if_else(Network %in% c(1, 2, 4) & abs(Single_DUT_Sum - DUT2_DUT3) < 0.25 * Single_DUT_Sum, TRUE, FALSE),
    Condition_8_256 = if_else(Network >= 8 & DUT2_DUT3 > Single_DUT_Sum, TRUE, FALSE)
  )

# Use of a Paired T-test
t_test_results <- t.test(results$DUT2_DUT3, results$Single_DUT_Sum, paired = TRUE)

# Pair T-test results
print(t_test_results)

# Calculation of T-test confidence interval
ci <- t_test_results$conf.int
mean_diff <- t_test_results$estimate

# Creating a line graph
line_plot <- ggplot(results, aes(x = factor(Network, levels = c(1, 2, 4, 8, 16, 32, 64, 128, 256)))) + 
  geom_line(aes(y = DUT2, color = "DUT2", group = 1)) +
  geom_line(aes(y = DUT3, color = "DUT3", group = 1)) +
  geom_line(aes(y = DUT2_DUT3, color = "DUT2-DUT3", group = 1)) +
  geom_point(aes(y = DUT2, color = "DUT2")) +
  geom_point(aes(y = DUT3, color = "DUT3")) +
  geom_point(aes(y = DUT2_DUT3, color = "DUT2-DUT3")) +
  labs(
    title = "Latency Comparison Between Single and Dual DUT Setups",
    x = "Number of Networks",
    y = "Median Latency",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("DUT2" = "blue", "DUT3" = "green", "DUT2-DUT3" = "red"))

print(line_plot)

# Line plot of Paired T-test results
t_test_plot <- ggplot(results, aes(x = factor(Network, levels = c(1, 2, 4, 8, 16, 32, 64, 128, 256)))) +
  geom_point(aes(y = DUT2_DUT3 - Single_DUT_Sum), color = "black") +
  geom_errorbar(aes(ymin = mean_diff - ci[2], ymax = mean_diff - ci[1]), width = 0.2) +
  labs(
    title = "Paired T-Test Results for Median Differences",
    x = "Number of Networks",
    y = "Median Difference (Dual DUT - Single DUT Sum)"
  ) +
  theme_minimal()

print(t_test_plot)

# Printing results
results_with_diff <- results %>%
  mutate(Difference = DUT2_DUT3 - Single_DUT_Sum)

print(results_with_diff)

# Export data to CSV file
write_csv(results_with_diff, "results_with_diff.csv")

