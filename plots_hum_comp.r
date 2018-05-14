library(readr)
library(tidyverse)
setwd("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/pdf2")

# X
x_human <- read_delim("../x_human_coverage_all_AR_corr.tsv",
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
# Translate into gene names
x_human = x_human %>%
    mutate(species = "human") %>%
    mutate(gene = case_when(
        AR == "6_0" ~ "GAGE4",
        AR == "24_0" ~ "CT47A4",
        AR == "26_0" ~ "CT45A5",
        AR == "27_0" ~ "SPANXB1",
        AR == "32_0" ~ "OPN1LW",
        AR == "DMD" ~ "DMD")) %>%
    dplyr::rename(sexchroms = sex) %>%
    mutate(sex = case_when(
        sexchroms == "XX" ~ "F",
        sexchroms == "XY" ~ "M"))
#validate sex
table(as.factor(x_human$sex), as.factor(x_human$sexchroms))


# jitter
for (igene in c("GAGE4", "CT47A4", "CT45A5", "SPANXB1", "OPN1LW", "DMD")) {
    ggplot(na.omit(x_human[x_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_jitter(width=0.1, height=0.00) +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("X: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/X_human_", igene, "_jit.pdf", sep=""), width=2, height=4)
    print(igene)
}

# boxplot
for (igene in c("GAGE4", "CT47A4", "CT45A5", "SPANXB1", "OPN1LW", "DMD")) {
    ggplot(na.omit(x_human[x_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        #geom_jitter(width=0.1, height=0.00) +
        geom_boxplot() +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("X: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/X_human_", igene, "_boxp.pdf", sep=""), width=2, height=4)
    print(igene)
}




# Y
y_human <- read_delim("../y_human_coverage_all_AR_corr.tsv",
                      "\t", escape_double = FALSE, trim_ws = TRUE)

# Translate into gene names
y_human = y_human %>%
    mutate(species = "human") %>%
    #mutate(gene = AR) %>%
    mutate(gene = case_when(
        AR == "chrY.background" ~ "chr. Y b.g.",
        AR != "chrY.background" ~ AR)) %>%
    dplyr::rename(sexchroms = sex) %>%
    mutate(sex = case_when(
        sexchroms == "XX" ~ "F",
        sexchroms == "XY" ~ "M"))
#validate sex
table(as.factor(y_human$AR), as.factor(y_human$gene))

# jitter
for (igene in c("BPY2", "CDY", "DAZ", "HSFY", "PRY", "TSPY", "XKRY", "chr. Y b.g.")) {
    ggplot(na.omit(y_human[y_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_jitter(width=0.1, height=0.00) +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("Y: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/Y_human_", igene, "_jit.pdf", sep=""), width=2, height=4)
    print(igene)
}


# jitter
for (igene in c("BPY2", "CDY", "DAZ", "HSFY", "PRY", "TSPY", "XKRY", "chr. Y b.g.")) {
    ggplot(na.omit(y_human[y_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        #geom_jitter(width=0.1, height=0.00) +
        geom_boxplot() +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("Y: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/Y_human_", igene, "_boxp.pdf", sep=""), width=2, height=4)
    print(igene)
}





