# this file is used in order to compare the read depths using either the human ac on monkeys, or the species specific ones.
# maybe the file should be called compare_acs.r instead

library(readr)
library(tidyverse)
spsp_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/full.csv")
human_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots_human_ac/full.csv")

# Done before normalization.
count_spsp = spsp_ac %>% 
    #select(ind, gene, count)
    group_by(gene) %>% 
    summarise(mean_cov = mean(count),
              `artificial chromosome` = "species specific")

count_human = human_ac %>% 
    # group_by(ind, gene, add = T) %>%
    # summarise(mean_cov = mean(count)) %>% 
    mutate(gene = case_when(
        # X chrom.
        gene == "6_0" ~ "GAGE4",
        gene == "24_0" ~ "CT47A4",
        gene == "26_0" ~ "CT45A5",
        gene == "27_0" ~ "SPANXB1",
        gene == "32_0" ~ "OPN1LW",
        gene == "DMD" ~ "DMD",
        # Y chrom
        gene == "AMELY" ~ "AMELY",
        gene == "BPY2" ~ "BPY2",
        gene == "CDY" ~ "CDY",
        gene == "PRY" ~ "PRY",
        gene == "RBMY1A1" ~ "RBMY1A1",
        gene == "TSPY" ~ "TSPY",
        #gene == "chrY.background" ~ "chr. Y b.g."
        )) %>% na.omit() %>% 
    group_by(gene) %>% 
    summarise(mean_cov = mean(count),
              `artificial chromosome` = "human") 


# otogether = count_spsp %>% 
#     full_join(count_human, by = "gene") %>% na.omit %>% 
#     mutate(rel_diff = mean_cov_spsp_ac / mean_cov_human_ac) %>% 
#     mutate(abs_diff = mean_cov_spsp_ac - mean_cov_human_ac) %>% 
#     arrange(rel_diff)

together = rbind(count_spsp, count_human) %>% 
    group_by(gene) %>% 
    mutate(mean_diff = mean(mean_cov)) %>% 
    arrange(mean_diff)


# 1 points
ggplot(together) +
    geom_line(aes(x = gene, y = mean_cov), color = "grey") +
    geom_point(aes(x = gene, y = mean_cov, color = `artificial chromosome`))

# 2 bars
ggplot(together) +
    #geom_line(aes(x = gene, y = mean_cov), color = "grey") +
    geom_col(aes(x = gene, y = mean_cov, fill = `artificial chromosome`),
             position = "dodge") +
    ylab("coverage: mean across all individuals")
    scale_x_discrete(limits = c(1,2,3))


    
# maybe I should do it with a boxplot (or jitter) for all individuals.
# facet_wrap it between species
    
box_spsp = spsp_ac# %>%

    
box_human = human_ac %>% 
    # group_by(ind, gene, add = T) %>%
    # summarise(mean_cov = mean(count)) %>% 
    mutate(gene = case_when(
        # X chrom.
        gene == "6_0" ~ "GAGE4",
        gene == "24_0" ~ "CT47A4",
        gene == "26_0" ~ "CT45A5",
        gene == "27_0" ~ "SPANXB1",
        gene == "32_0" ~ "OPN1LW",
        gene == "DMD" ~ "DMD",
        # Y chrom
        gene == "AMELY" ~ "AMELY",
        gene == "BPY2" ~ "BPY2",
        gene == "CDY" ~ "CDY",
        gene == "PRY" ~ "PRY",
        gene == "RBMY1A1" ~ "RBMY1A1",
        gene == "TSPY" ~ "TSPY"
        #gene == "chrY.background" ~ "chr. Y b.g."
    )) %>% na.omit()
    # kan man ikke bare bruge select til at udvælge rækker?
    # Fjern orangutanger!

# I should really consider removing the Y genes from the gorilla in the human ac file. They shouldn't be compared to spsp_ac, as they don't exist over there.
box_human = box_human[! (box_human$species == "gorilla" & box_human$chrom == "Y") # fjern gorilla Y
                      & box_human$species != "orang" # fjern orangutaner
                      & !(box_human$species == "gorilla" & box_human$gene == "SPANXB1" ),] # fjern spanxb1 fra gorilla

box_together

ggplot()