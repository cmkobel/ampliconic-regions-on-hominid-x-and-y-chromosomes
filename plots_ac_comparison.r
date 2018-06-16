# this file is used in order to compare the read depths using either the human ac on monkeys, or the species specific ones.
# maybe the file should be called compare_acs.r instead

library(readr)
library(tidyverse)
spsp_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots/full.csv")
human_ac <- read_csv("/Volumes/GenomeDK/simons/faststorage/people/carl/coverage/plots_human_ac/full_zeroes_inserted23b.csv")

# Done before normalization.
count_spsp = spsp_ac %>% 
    #select(ind, gene, count)
    group_by(chrom, gene, species) %>% 
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
        gene == "TSPY" ~ "TSPY"
        #gene == "chrY.background" ~ "chr. Y b.g."
        )) %>% na.omit() %>% 
    group_by(chrom, gene, species) %>% 
    summarise(mean_cov = mean(count),
              `artificial chromosome` = "human")


# otogether = count_spsp %>% 
#     full_join(count_human, by = "gene") %>% na.omit %>% 
#     mutate(rel_diff = mean_cov_spsp_ac / mean_cov_human_ac) %>% 
#     mutate(abs_diff = mean_cov_spsp_ac - mean_cov_human_ac) %>% 
#     arrange(rel_diff)

together = rbind(count_spsp, count_human) %>% 
    group_by(gene) %>% 
    mutate(mean_diff = mean(mean_cov)) %>% # Bruges ingen steder?
    arrange(mean_diff) # Egentlig ikke nødvendigt at sortere.

# together[together$chrom == "X" & together$`artificial chromosome` == "species specific",] %>% 
#     View()

# 1 points, this plot is too weird looking
# ggplot(together) +
#     geom_line(aes(x = gene, y = mean_cov), color = "grey") +
#     geom_point(aes(x = gene, y = mean_cov, color = `artificial chromosome`))

# 2 bars
ggplot(together[together$species = "chimp",]) +
    #geom_line(aes(x = gene, y = mean_cov), color = "grey") +
    geom_col(aes(x = gene, y = mean_cov, fill = `artificial chromosome`),
             position = "dodge", width = 0.5) +
    labs(title = "All individuals, X and Y chromosomes:" , y = "coverage: mean across all individuals", subtitle="comparison of two artificial chomosomes") +
    scale_x_discrete(limits = c("GAGE4", "SPANXB1", "CT47A4", "CT45A5", "OPN1LW", "AMELY", "BPY2", "PRY", "CDY", "RBMY1A1", "TSPY" )) +
    theme(title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    ggsave(file = "plots/compare/all.pdf", width = 10, height = 6) 

# print(
#     grid.arrange(
#         ggplot(plot_df) + 
#             geom_point(aes(x=POSITION, y=iHS), size=0.03) +
#             xlab("") +
#             ylab("iHS") + ggtitle(title),
#         ggplot(plot_df) + 
#             geom_point(aes(x=POSITION, y=`-log10(p-value)`), size=0.03) +
#             xlab("chromosome X position") +
#             ylab("-log10( p-value )"),
#         layout_matrix = rbind(c(1),c(2))
#     )
# )


    
  # maybe I should do it with a boxplot (or jitter) for all individuals.
# facet_wrap it between species
    
box_spsp = spsp_ac %>%
    mutate(`artificial chromosome` = "species specific")

    
box_human = human_ac %>% 
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

# I should really consider removing the Y genes from the gorilla in the human ac file. They shouldn't be compared to spsp_ac, as they don't exist over there.
box_human = box_human[! (box_human$species == "gorilla" & box_human$chrom == "Y") # fjern gorilla Y
                      & box_human$species != "orang" # fjern orangutaner
                      & !(box_human$species == "gorilla" & box_human$gene == "SPANXB1" ),] # fjern spanxb1 fra gorilla


box_together = rbind(box_human, box_spsp)

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
box_together_mean = box_together %>% 
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
        ggplot(box_together_mean[box_together_mean$species == ispecies & box_together_mean$chrom == ichrom & !(box_together_mean$species == "gorilla" & box_together_mean$gene == "SPANXB1"),], aes(x = gene, y = mean_count)) + 
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

