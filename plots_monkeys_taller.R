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



df = df %>%
    group_by(chrom, ind, add = T) %>%
    mutate(norm_count = count / df[df$gene == chrom2control(chrom) & df$ind == ind,]$count)
View(df)


cutoff <- data.frame( x = c(-Inf, Inf), y = 50, cutoff = factor(50))


library(scales)
# X_species
for(ichrom in c("X")) {
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
            theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5))
        ggplot2::ggsave(paste(ichrom, "_species_", ispecies, ".pdf", sep=""), width=2.8, height=5)
        print(ispecies)
        #print(get_non_amp_reg(ichrom))
    }
}
# Y_species
for(ichrom in c("Y")) {
    for (ispecies in unique(df$species)) {
        ggplot(df[df$chrom==ichrom & df$species==ispecies & df$sex != "F",], aes(x=gene, y=norm_count)) +
            #geom_point(size=3) +
            geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
            geom_jitter(width=0.15, height=0.00, color = "#00BFC4") +
            scale_y_continuous(breaks=pretty_breaks(n = 10), minor_breaks=pretty_breaks(n = 40)) +
            #expand_limits(y = 0) +
            #facet_wrap(~ ind) + 
            #scale_y_continuous(limits = c(0,max(df$))) +
            ggtitle(paste(ichrom, ": ", expand_species(ispecies), sep="")) +
            labs(y=paste("number of copies"), x="gene") +
            theme(axis.text.x = element_text(angle=90, hjust=1, vjust = 0.5))
        ggplot2::ggsave(paste(ichrom, "_species_", ispecies, ".pdf", sep=""), width=2.1, height=5)
        print(ispecies)
        #print(get_non_amp_reg(ichrom))
    }
}






# chrom gene X
for(ichrom in c("X")) {
    for (igene in unique(df[df$chrom==ichrom,]$gene)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$gene==igene & !(df$chrom == "Y" & df$sex == "F"),], aes(x=species, y=norm_count, color=sex)) +
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
#chrom gene Y
for(ichrom in c("Y")) {
    for (igene in unique(df[df$chrom==ichrom,]$gene)) { # for each unique gene-name
        ggplot(df[df$chrom==ichrom & df$gene==igene & !df$sex == "F",], aes(x=species, y=norm_count)) +
            #geom_point(size=3) +
            geom_hline(aes(yintercept=1, linetype=cutoff), data=cutoff, show.legend=F, size = 0.2) +
            geom_jitter(width=0.1, height=0.00, color = "#00BFC4") +
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
    #mutate(id = paste(sex, gene)) %>% 
    arrange(species, chrom, sex, gene)

#import human

stats_hum <- read_csv("stats_hum.csv") %>% 
    #mutate(id = paste(sex, gene))
    select(sex, gene, median)

stats_merged = stats %>%
    left_join(stats_hum, by = c("sex", "gene"), suffix = c("", "_human")) %>% 
    rename(human_median = median_human) %>% 
    arrange(species, chrom, sex, gene)

library(readr)
write_excel_csv(stats_merged, "stats_merged.csv", na = "NA", append = FALSE)



#sort: median, sex, chrom, species