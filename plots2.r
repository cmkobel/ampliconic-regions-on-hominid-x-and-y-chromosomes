library(readr)
library(tidyverse)

#setwd("..")
#setwd("plots")
#getwd()
df <- read_csv("full.csv")[,-1]
#View(df)


get_non_amp_reg = function(chromosome) {
    if(chromosome == "X") {
        return("DMD")}
    else if(chromosome == "Y") {
        return("AMELY")}
    else
        return(NA)}

df = df %>%
    group_by(ind, chrom) %>%
    mutate(normalized = count / df[df$gene == get_non_amp_reg(chrom) & df$ind == ind,]$count)
View(df)

setwd("png2")
for(ichrom in c("X", "Y")) {
    for (igene in unique(df[df$chrom==ichrom,]$gene)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$gene==igene,], aes(x=species, y=count)) +
            geom_point() +
            ggtitle(gene) +
            labs(y=paste("CN / |", get_non_amp_reg(ichrom), "|", sep=""), x="species")
        ggplot2::ggsave(paste(ichrom, "_gene_", igene, ".png", sep="")) # cowplot is masking ggsave

        print(igene)
        #print(get_non_amp_reg(ichrom))
    }
}

