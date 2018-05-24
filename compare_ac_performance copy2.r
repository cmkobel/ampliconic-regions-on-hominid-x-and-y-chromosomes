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
    
filtered_spsp = spsp_ac %>%
    mutate(`artificial chromosome` = "species-specific")

    
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
        gene == "DMD" ~ "DMD",
        # Y chrom
        gene == "AMELY" ~ "AMELY",
        gene == "BPY2" ~ "BPY2",
        gene == "CDY" ~ "CDY",
        gene == "PRY" ~ "PRY",
        gene == "RBMY1A1" ~ "RBMY1A1",
        gene == "TSPY" ~ "TSPY"
        #gene == "chrY.background" ~ "chr. Y b.g."
    )) %>% na.omit() %>% 
    # kan man ikke bare bruge select til at udvælge rækker?
    # Fjern orangutanger!
    mutate(`artificial chromosome` = "human")

# Det her kan sikkert også gøres med select i linjerne over.
filtered_human = filtered_human[! (filtered_human$species == "gorilla" & filtered_human$chrom == "Y") # fjern gorilla Y
                      & filtered_human$species != "orang" # fjern orangutaner
                      & !(filtered_human$species == "gorilla" & filtered_human$gene == "SPANXB1" ),] # fjern spanxb1 fra gorilla


together = rbind(filtered_human, filtered_spsp) %>% 
    group_by(species, sex, chrom, gene, `artificial chromosome`) %>% 
    summarise(mean_cov = mean(count))

# together[together$species == ispecies & together$`artificial chromosome` == "species-specific" & together$chrom == "Y",] %>% 
#     View()

ispecies = "chimp"
ggplot(together[together$species == ispecies,], aes(x = gene, y = mean_cov, fill = `artificial chromosome`)) +
    geom_col(position = "dodge", width = 0.5) +
    geom_text(aes(y = mean_cov + 3, label = round(mean_cov)), position = position_dodge(width = 0.5), color = rgb(0.5, 0.5, 0.5)) +
    facet_grid(sex ~ .) +
    scale_x_discrete(limits = c("GAGE4", "CT47A4", "OPN1LW", "SPANXB1", "CT45A5", "BPY2", "PRY", "CDY", "RBMY1A1", "TSPY" )) +
    labs(title = paste0(abbrev2spec(ispecies), " chromosomes X and Y"),
         subtitle="Comparison of artifical chromosomes",
         y = "mean coverage across all individuals") +
    theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




            #scale_x_discrete(limits = c("BPY2", "PRY", "DMD", "GAGE4", "CDY", "SPANXB1", "CT47A4", "CT45A5", "OPN1LW", "RBMY1A1", "TSPY")) +
            facet_wrap(~ sex) +
            labs(title = paste0(abbrev2spec(ispecies), ", ", ichrom, " chromosome"), subtitle="Comparison of artificial chromosomes", y = "coverage: mean across all individuals") + #,
            # x="Weather    stations", 
            # y="Accumulated Rainfall [mm]",
            # title="Rainfall",
            #xlab("gene") +
            #ylab("coverage: mean across all individuals") + 
            #ggtitle(paste0(abbrev2spec(ispecies), ", ", ichrom, " chromosome: comparison of different artifical chromosomes")) +
            theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) +
            ggsave(paste0("plots/compare/", ispecies, "_", ichrom, ".pdf"), width = 5, height = 4)
        # find den samme bredde som de andre figurer har, og brug den til at exportere med. Indstil dodgewidth so det ser sejt ud.
    }
}


# 
# ggplot(box_together[box_together$species == "chimp",]) + 
#     geom_boxplot(aes(x = gene, y = count, color = `artificial chromosome`)) + 
#     scale_x_discrete(limits = c("AMELY", "BPY2", "PRY", "DMD", "GAGE4", "CDY", "SPANXB1", "CT47A4", "CT45A5", "OPN1LW", "RBMY1A1", "TSPY")) +
#     xlab("gene") +
#     ylab("coverage: mean across all individuals") + 
#     ggtitle("Comparison of methods (Chimpanzee)") + 
#     facet_wrap(~ sex) +
#     theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))#, hjust=1, size=3)) +
# 
# 
# ggplot(box_together[box_together$species == "gorilla",]) + 
#     #geom_boxplot(aes(x = gene, y = count, color = `artificial chromosome`)) + 
#     geom_line(aes(x = gene, y = count)) +
#     geom_point(aes(x = gene, y = count, color = `artificial chromosome`)) + 
#     xlab("gene") +
#     ylab("coverage: mean across all individuals") + 
#     ggtitle("Comparison of methods (Gorilla)") + 
#     facet_wrap(~ sex) +
#     theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))#, hjust=1, size=3))


# Hvad fanden er det her til?
# box_together[box_together$`artificial chromosome` == "species specific",] %>% 
#     group_by(gene, `artificial chromosome`) %>% 
#     summarise(mean = mean(count)) %>% 
#     View()

# Because the variance is roughly proportional with the copy number, I decided to only plot the means
# I selected the mean, because the distributions weren't far off
# Jeg vil gerne dele det op i arter med facetter af sex, men med gennemsnit i stedet for 

# lav df til at plotte means kun:
together = box_together %>% 
    group_by(species, sex, chrom, gene, `artificial chromosome`) %>%
    summarise(mean_count = mean(count))

# animal = "chimp"
# ggplot(box_together_mean[box_together_mean$species == animal,], aes(x = gene, y = mean_count)) + 
#     geom_line(color = "grey") +
#     geom_point(aes(color = `artificial chromosome`), position=position_dodge(width = .3), size = 3.3) +# , height = 0, width = 1) +
#     #scale_x_discrete(limits = c("BPY2", "AMELY", "PRY", "DMD", "GAGE4", "CDY", "SPANXB1", "CT47A4", "CT45A5", "OPN1LW", "RBMY1A1", "TSPY")) +
#     facet_wrap(~ sex) +
#     xlab("gene") +
#     ylab("coverage: mean across all individuals") + 
#     ggtitle(paste("Comparison of methods,", animal)) +
#     theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
# # find den samme bredde som de andre figurer har, og brug den til at exportere med. Indstil dodgewidth so det ser sejt ud.

abbrev2spec = function(abbrev) {
    if (abbrev == "gorilla") return ("Gorilla")
    else if (abbrev == "chimp") return ("Chimpanzee")
}



# maybe bars are better?
# det her er super.
ispecies = "chimp"
ichrom = "Y"

for (ispecies in c("chimp", "gorilla")) {
    for (ichrom in c("X", "Y")) {
        ggplot(together[together$species == ispecies & together$chrom == ichrom & !(together$species == "gorilla" & together$gene == "SPANXB1"),], aes(x = gene, y = mean_count)) + 
            geom_bar(aes(fill = `artificial chromosome`, width = .5), stat = "identity", position = "dodge") +
            #scale_x_discrete(limits = c("BPY2", "PRY", "DMD", "GAGE4", "CDY", "SPANXB1", "CT47A4", "CT45A5", "OPN1LW", "RBMY1A1", "TSPY")) +
            facet_wrap(~ sex) +
            labs(title = paste0(abbrev2spec(ispecies), ", ", ichrom, " chromosome"), subtitle="Comparison of different artifical chromosomes", y = "coverage: mean across all individuals") + #,
                # x="Weather    stations", 
                # y="Accumulated Rainfall [mm]",
                # title="Rainfall",
            #xlab("gene") +
            #ylab("coverage: mean across all individuals") + 
            #ggtitle(paste0(abbrev2spec(ispecies), ", ", ichrom, " chromosome: comparison of different artifical chromosomes")) +
            theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) +
            ggsave(paste0("plots/compare/", ispecies, "_", ichrom, ".pdf"), width = 5, height = 4)
        # find den samme bredde som de andre figurer har, og brug den til at exportere med. Indstil dodgewidth so det ser sejt ud.
    }
}

# Og så en overall.

