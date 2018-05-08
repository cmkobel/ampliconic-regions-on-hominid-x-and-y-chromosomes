library(readr)
library(tidyverse)
setwd("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/pdf2")

# X
x_human <- read_delim("../compare_human/x_human_coverage_all_AR_corr.tsv",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
# Translate into gene names
x_human = x_human %>%
    mutate(species = "human") %>%
    mutate(gene = case_when(
        AR == "6_0" ~ "GAGE4",
        AR == "24_0" ~ "CT47A4",
        AR == "26_0" ~ "CT45A5",
        AR == "27_0" ~ "SPANXB1",
        AR == "32_0" ~ "OPN1LW")) %>%
    rename(oldsex = sex) %>%
    mutate(sex = case_when(
        oldsex == "XX" ~ "F",
        oldsex == "XY" ~ "M"))
#validate sex
table(as.factor(x_human$sex), as.factor(x_human$oldsex))


#boxplot(x_human[x_human$gene == "GAGE4",]$normalized_cov)

for (igene in c("GAGE4", "CT47A4", "CT45A5", "SPANXB1", "OPN1LW", "DMD")) {

    ggplot(na.omit(x_human[x_human$gene == "GAGE4",]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_jitter(width=0.1, height=0.00) +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("X: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("X_human_", igene, "_jit.pdf", sep=""), width=2, height=4)
    print(igene)

}


# Y
y_human <- read_delim("../compare_human/y_human_coverage_all_AR_corr.tsv",
                      "\t", escape_double = FALSE, trim_ws = TRUE)


