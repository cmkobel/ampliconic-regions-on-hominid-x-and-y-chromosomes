# This file creates the plots from elises results on humans.
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


# overview
for(i in 1:1) {
    ggplot(x_human[! is.na(x_human$gene) & ! is.na(x_human$sex),], aes(x=gene, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
        geom_jitter(width=0.15, height=0.00, size = 0.04) +
        scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
        #facet_wrap(~ ind) + 
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle("X: Human") +
        labs(y=paste("number of copies"), x="gene") +
        theme(axis.text.x = element_text(angle=90, hjust=1))
    
    ggplot2::ggsave("human/X_human.pdf", width=2.8, height=5)
    print("x human overview")
    #print(get_non_amp_reg(ichrom))
}


# jitter
for (igene in unique(na.omit(x_human$gene))) {
    ggplot(na.omit(x_human[x_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
        geom_jitter(width=0.1, height=0.00, size = 0.25) +
        scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("X: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/X_human_", igene, "_jit.pdf", sep=""), width=2, height=5)
    print(igene)
}

# boxplot
# for (igene in unique(na.omit(x_human$gene))) { # c kan erstattes med unique
#     ggplot(na.omit(x_human[x_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
#         #geom_point(size=3) +
#         #geom_jitter(width=0.1, height=0.00) +
#         geom_boxplot() +
#         #scale_y_continuous(limits = c(0,max(df$))) +
#         ggtitle(paste("X: ", igene, sep="")) +
#         labs(y=paste("number of copies"), x="species")
#     ggplot2::ggsave(paste("human/X_human_", igene, "_boxp.pdf", sep=""), width=2, height=4)
#     print(igene)
# }




# Y
y_human <- read_delim("../y_human_coverage_all_AR_corr.tsv",
                      "\t", escape_double = FALSE, trim_ws = TRUE)

# Translate into gene names
y_human = y_human %>%
    mutate(species = "human") %>%
    mutate(gene = case_when(
        AR == "AMELY" ~ "AMELY",
        AR == "BPY2" ~ "BPY2",
        AR == "CDY" ~ "CDY",
        AR == "PRY" ~ "PRY",
        AR == "RBMY1A1" ~ "RBMY1A1",
        AR == "TSPY" ~ "TSPY",
        AR == "chrY.background" ~ "chr. Y b.g.")) %>%
    dplyr::rename(sexchroms = sex) %>%
    mutate(sex = case_when(
        sexchroms == "XX" ~ "F",
        sexchroms == "XY" ~ "M"))
#validate sex
table(as.factor(y_human$AR), as.factor(y_human$gene))

# survey BPY2
y_human %>% 
    filter(gene == "BPY2" & sex == "M") %>% 
    View()

# overview
for(i in 1:1) {
    ggplot(y_human[! is.na(y_human$gene),], aes(x=gene, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
        geom_jitter(width=0.15, height=0.00, size = 0.04) +
        scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
        #coord_trans(y="log10") +
        #facet_wrap(~ ind) + 
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle("Y: Human") +
        labs(y=paste("number of copies"), x="gene") +
        theme(axis.text.x = element_text(angle=90, hjust=1)) 
    ggplot2::ggsave("human/Y_human.pdf", width=2.8, height=5)
    print("y human overview")
}



# jitter
for (igene in unique(na.omit(y_human$gene))) {
    ggplot(na.omit(y_human[y_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
        #geom_point(size=3) +
        geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
        geom_jitter(width=0.1, height=0.00, size = 0.25) +
        scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
        #scale_y_continuous(limits = c(0,max(df$))) +
        ggtitle(paste("Y: ", igene, sep="")) +
        labs(y=paste("number of copies"), x="species")
    ggplot2::ggsave(paste("human/Y_human_", igene, "_jit.pdf", sep=""), width=2, height=5)
    print(igene)
}


# # boxplot
# for (igene in unique(na.omit(y_human$gene))) {
#     ggplot(na.omit(y_human[y_human$gene == igene,]), aes(x=species, y=normalized_cov, color=sex)) +
#         #geom_point(size=3) +
#         #geom_jitter(width=0.1, height=0.00) +
#         geom_boxplot() +
#         #scale_y_continuous(limits = c(0,max(df$))) +
#         ggtitle(paste("Y: ", igene, sep="")) +
#         labs(y=paste("number of copies"), x="species")
#     ggplot2::ggsave(paste("human/Y_human_", igene, "_boxp.pdf", sep=""), width=2, height=4)
#     print(igene)
# }





