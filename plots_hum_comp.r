library(readr)
library(tidyverse)

# X
x_human <- read_delim("plots/compare_human/x_human_coverage_all_AR_corr.tsv",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
# Translate into gene names
x_human = x_human %>%
    mutate(gene = case_when(
        AR == "6_0" ~ "GAGE4",
        AR == "24_0" ~ "CT47A4",
        AR == "26_0" ~ "CT45A5",
        AR == "27_0" ~ "SPANXB1",
        AR == "32_0" ~ "OPN1LW",
    ))
#boxplot(x_human[x_human$gene == "GAGE4",]$normalized_cov)


# Y
y_human <- read_delim("plots/compare_human/y_human_coverage_all_AR_corr.tsv",
                      "\t", escape_double = FALSE, trim_ws = TRUE)

