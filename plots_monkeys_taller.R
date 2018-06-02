library(readr)
library(tidyverse)


setwd("plots/pdf2")
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
    mutate(norm_count = count / df[df$gene == chrom2control(chrom) & df$ind == ind,]$count)
View(df)


cutoff <- data.frame( x = c(-Inf, Inf), y = 50, cutoff = factor(50))


library(scales)
# chrom species # jeg kan åbenbart ikke få lov til at wrappe.
for(ichrom in unique(df$chrom)) {
    for (ispecies in unique(df$species)) {
        ggplot(df[df$chrom==ichrom & df$species==ispecies,], aes(x=gene, y=norm_count, color=sex)) +
            #geom_point(size=3) +
            geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
            geom_jitter(width=0.15, height=0.00) +
            scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
            #expand_limits(y = 0) +
            #facet_wrap(~ ind) + 
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(paste(ichrom, ": ", expand_species(ispecies), sep="")) +
            labs(y=paste("number of copies"), x="gene") +
            theme(axis.text.x = element_text(angle=90, hjust=1))
        ggplot2::ggsave(paste(ichrom, "_species_", ispecies, ".pdf", sep=""), width=2.8, height=5)
        print(ispecies)
        #print(get_non_amp_reg(ichrom))
    }
}


# chrom gene
for(ichrom in unique(df$chrom)) {
    for (igene in unique(df[df$chrom==ichrom,]$gene)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$gene==igene,], aes(x=species, y=norm_count, color=sex)) +
            #geom_point(size=3) +
            geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
            geom_jitter(width=0.1, height=0.00) +
            scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(paste(ichrom, ": ", igene, sep="")) +
            labs(y=paste("number of copies"), x="species")
        ggplot2::ggsave(paste(ichrom, "_gene_", igene, ".pdf", sep=""), width=2.8, height=5)
        print(igene)
        #print(get_non_amp_reg(ichrom))
    }
}

stats = df %>% 
    group_by(species, chrom, sex, gene) %>% 
    summarise(min = round(min(norm_count), 2),
              median = round(median(norm_count), 2),
              max = round(max(norm_count), 2),
              sd = round(sd(norm_count), 2),
              n_ind.s = length(norm_count)) %>% 
    arrange(species, chrom, sex, desc(median))

library(readr)
write_excel_csv(stats, "stats.csv", na = "NA", append = FALSE)



#sort: median, sex, chrom, species