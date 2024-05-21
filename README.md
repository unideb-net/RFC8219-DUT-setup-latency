# Single DUT and dual DUT setup comparison based on RFC 8219
## What is this?

The following R code performs a Paired T-test based statistical analysis for our single DUT and dual DUT latency measurements as follows:

1. **Read and Process Data:** read data and calculate medians for each network configuration.
2. **Delay Sum:** Calculate the Single DUT Sum by summing the DUT2 and DUT3 medians.
3. **Analysis and Supplementary Variables:** Calculate the different conditions.
4. **Paired T-test:** perform a paired t-test between the Dual DUT and Single DUT Sum data using t.test().
5. **Calculate Confidence Interval and Mean of Difference:** Plot the results and calculate the confidence interval.
6. **Line plot:** produce a line plot of the original data.
7. **T-Test Diagram:** Visualize the results of the t-test, plotting the mean of the differences and the 95% confidence intervals.
8. **Plot Results:** Plot results and differences and export to a CSV file.

**Note:** 
- Part of the R script was generated with **ChatGPT-4o**.
- The diagrams were created with **RStudio** version 2024.04.0+735.
