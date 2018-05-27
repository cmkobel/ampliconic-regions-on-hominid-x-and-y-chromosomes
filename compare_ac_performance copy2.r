# this file is used in order to compare the read depths using either the human ac on monkeys, or the species specific ones.
# This is the second iteration, because the first one became too cluttered

library(readr)
library(tidyverse)
library(ggplot2)
spsp_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/full.csv")
human_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots_human_ac/full_zeroes_inserted23b.csv")


abbrev2spec = function(abbrev) {
    if (abbrev == "gorilla") return ("Gorilla")
    else if (abbrev == "chimp") return ("Chimpanzee")
}

abbrev2gender = function(abbrev) {
    if (abbrev == "F") return ("female")
    else if (abbrev == "M") return ("male")
}

filtered_spsp = spsp_ac %>%
    mutate(`artificial_chromosome` = "species-specific") %>% 
    mutate(gender = case_when(
        sex == "F" ~ "female (XX)",
        sex == "M" ~ "male (XY)"
    ))

    
filtered_human = human_ac %>% 
    # group_by(ind, gene, add = T) %>%
    # summarise(mean_cov = mean(count)) %>% 
    mutate(gene = case_when( # omskriv gene kolonnen
        # X chrom.
        gene == "6_0" ~ "GAGE4",
        gene == "24_0" ~ "CT47A4",
        gene == "26_0" ~ "CT45A5",
        gene == "27_0" ~ "SPANXB1",
        gene == "32_0" ~ "OPN1LW",
        gene == "DMD" ~ "DMD", # hvorfor er DMD ikke med??
        # Y chrom
        gene == "AMELY" ~ "AMELY",
        gene == "BPY2" ~ "BPY2",
        gene == "CDY" ~ "CDY",
        gene == "PRY" ~ "PRY",
        gene == "RBMY1A1" ~ "RBMY1A1",
        gene == "TSPY" ~ "TSPY"
        #gene == "chrY.background" ~ "chr. Y b.g."
    )) %>% na.omit() %>% 
    mutate(gender = case_when(
        sex == "F" ~ "female (XX)",
        sex == "M" ~ "male (XY)"
    )) %>%
    # kan man ikke bare bruge select til at udvælge rækker?
    # Fjern orangutanger!
    mutate(`artificial_chromosome` = "human")

# Det her kan sikkert også gøres med select i linjerne over.
filtered_human = filtered_human[! (filtered_human$species == "gorilla" & filtered_human$chrom == "Y") # fjern gorilla Y
                      & filtered_human$species != "orang" # fjern orangutaner
                      & !(filtered_human$species == "gorilla" & filtered_human$gene == "SPANXB1" ),] # fjern spanxb1 fra gorilla


# add the two cromosome methods
together = rbind(filtered_human, filtered_spsp) %>% 
    group_by(species, gender, chrom, gene, `artificial_chromosome`) %>% 
    summarise(mean_cov = mean(count))

# together[together$species == ispecies & together$`artificial_chromosome` == "species-specific" & together$chrom == "Y",] %>% 
#     View()


ggplot(together[together$species == "chimp",], aes(x = gene, y = mean_cov, fill = `artificial_chromosome`)) +
    geom_col(position = "dodge", width = 0.5) +
    geom_text(aes(y = mean_cov + 5, label = round(mean_cov)), position = position_dodge(width = 0.5), color = rgb(0.45, 0.45, 0.45), size = 2.7) +
    facet_grid(gender ~ .) +
    scale_x_discrete(limits = c("DMD", "GAGE4", "CT47A4", "OPN1LW", "SPANXB1", "CT45A5", "AMELY", "BPY2", "PRY", "CDY", "RBMY1A1", "TSPY")) +
    labs(title = paste0("Chimpanzee X and Y-linked genes"),
         subtitle="Comparison of artifical chromosomes",
         y = "mean coverage across all individuals") +
    theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggsave(file = paste0("plots/compare/new/all_chimp.pdf"), width = 10, height = 5) 

    
ggplot(together[together$species == "gorilla",], aes(x = gene, y = mean_cov, fill = `artificial_chromosome`)) +
    geom_col(position = "dodge", width = 0.5) +
    geom_text(aes(y = mean_cov + 7, label = round(mean_cov)), position = position_dodge(width = 0.5), color = rgb(0.45, 0.45, 0.45), size = 2.7) +
    facet_grid(gender ~ .) +
    #scale_x_discrete(limits = c("DMD", "GAGE4", "CT47A4", "CT45A5",  "OPN1LW")) +
    scale_x_discrete(limits = c("DMD", "GAGE4", "CT47A4", "OPN1LW", "CT45A5")) +
    labs(title = paste0("Gorilla X-linked genes"),
         subtitle="Comparison of artifical chromosomes",
         y = "mean coverage across all individuals") +
    theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    ggsave(file = paste0("plots/compare/new/all_gorilla.pdf"), width = 5, height = 5) 

# In chimpanzee, the mean of the pairwise difference for each individual is
library(tidyr)
pairwise_stat = function(ispec, ichrom, isex) {
    pairwise_dist = rbind(filtered_human, filtered_spsp)[-1] %>% 
        filter(species == ispec,
               chrom == ichrom,
               sex == isex) %>% 
        group_by(ind, gene, count, `artificial_chromosome`) %>%
        summarise() %>% 
        spread(artificial_chromosome, count) %>% 
        mutate(abs_diff = `species-specific` - human,
               rel_diff = `species-specific` / human)
    return(pairwise_dist)
}

mean(pairwise_stat("chimp", "Y", "F")$abs_diff)
mean(pairwise_stat("chimp", "X", "F")$rel_diff)

stats = tibble(species = c("chimp", "chimp", "chimp", "chimp", "gorilla", "gorilla"),
               sex = c("F", "M", "F", "M", "F", "M"),
               chromosome = c("X", "X", "Y", "Y", "X", "X"),
               `rel diff` = c(mean(pairwise_stat("chimp", "X", "F")$rel_diff), 
                              mean(pairwise_stat("chimp", "X", "M")$rel_diff), 
                              mean(pairwise_stat("chimp", "Y", "F")$rel_diff), 
                              mean(pairwise_stat("chimp", "Y", "M")$rel_diff), 
                              mean(pairwise_stat("gorilla", "X", "F")$rel_diff), 
                              mean(pairwise_stat("gorilla", "X", "M")$rel_diff)),
               `abs diff` = c(mean(pairwise_stat("chimp", "X", "F")$abs_diff),
                              mean(pairwise_stat("chimp", "X", "M")$abs_diff),
                              mean(pairwise_stat("chimp", "Y", "F")$abs_diff),
                              mean(pairwise_stat("chimp", "Y", "M")$abs_diff),
                              mean(pairwise_stat("gorilla", "X", "F")$abs_diff),
                              mean(pairwise_stat("gorilla", "X", "M")$abs_diff)))
# Det giver ikke mening at indsætte det absolutte forskel, da den ikke tager højde for     


pairwise_mean_across = pairwise_dist %>% 
    group_by(species) %>% 
    summarise(mean = mean(dist))




