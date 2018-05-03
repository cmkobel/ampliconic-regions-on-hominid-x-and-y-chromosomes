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
    mutate(norm_count = count / df[df$gene == get_non_amp_reg(chrom) & df$ind == ind,]$count)
View(df)

setwd("png2")

# overview
# del ind plotsene op i chromosom (så det er de samme gener sammen)


# chrom species
for(ichrom in unique(df$chrom)) {
    for (ispecies in unique(df$species)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$species==ispecies,], aes(x=gene, y=norm_count, color=sex)) +
            #geom_point(size=3) +
            geom_jitter(width=0.1, height=0.00) +
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(igene) +
            labs(y=paste("number of copies"), x="species")
        ggplot2::ggsave(paste(ichrom, "_species_", ispecies, ".png", sep=""), width=3, height=4)
        print(ispecies)
        #print(get_non_amp_reg(ichrom))
    }
}


# chrom gene
for(ichrom in unique(df$chrom)) {
    for (igene in unique(df[df$chrom==ichrom,]$gene)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$gene==igene,], aes(x=species, y=norm_count, color=sex)) +
            #geom_point(size=3) +
            geom_jitter(width=0.1, height=0.00) +
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(igene) +
            labs(y=paste("number of copies"), x="species")
        ggplot2::ggsave(paste(ichrom, "_gene_", igene, ".png", sep=""), width=3, height=4)
        print(igene)
        #print(get_non_amp_reg(ichrom))
    }
}
