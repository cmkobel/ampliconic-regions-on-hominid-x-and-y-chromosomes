library(readr)
library(tidyverse)


setwd("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/pdf2")
#getwd()
#setwd("..")
df <- read_csv("../full.csv")[,-1]
#setwd("pdf2")
#View(df)





chrom2control = function(chromosome) {
    if(chromosome == "X") {
        return("DMD")}
    else if(chromosome == "Y") {
        return("AMELY")}
    else
        return(NA)}

expand_species = function(species) {
    if(species == "chimp") {
        return("Chimpanzee")}
    else if(species == "gorilla") {
        return("Gorilla")}
    else
        return(NA)}


# er det her en test?
df = df %>%
    group_by(chrom, ind, add = T) %>%
    mutate(norm_count = df[df$gene == chrom2control(chrom) & df$ind == ind,]$count)
View(norm_df)
 


# chrom species # jeg kan åbenbart ikke få lov til at wrappe.
for(ichrom in unique(df$chrom)) {
    for (ispecies in unique(df$species)) {
        ggplot(df[df$chrom==ichrom & df$species==ispecies,], aes(x=gene, y=norm_count, color=sex)) +
            #geom_point(size=3) +
            geom_jitter(width=0.15, height=0.00) +
            #facet_wrap(~ ind) + 
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(paste(ichrom, ": ", expand_species(ispecies), sep="")) +
            labs(y=paste("number of copies"), x="gene") +
            theme(axis.text.x = element_text(angle=90, hjust=1)) 
        ggplot2::ggsave(paste(ichrom, "_species_", ispecies, ".pdf", sep=""), width=2.8, height=4)
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
            ggtitle(paste(ichrom, ": ", igene, sep="")) +
            labs(y=paste("number of copies"), x="species")
        ggplot2::ggsave(paste(ichrom, "_gene_", igene, ".pdf", sep=""), width=2.8, height=4)
        print(igene)
        #print(get_non_amp_reg(ichrom))
    }
}

