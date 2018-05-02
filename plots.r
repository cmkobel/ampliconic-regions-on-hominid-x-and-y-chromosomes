library(readr)
library(tidyverse)

df <- read_csv("plots/full.csv")
View(df)


# All values should be normalized in regard to:
#   DMD for the X chromosome
#   chrY.background for the Y chromosome
##Yes :)

# old version (human ac's)
# xchr_normalized <- df[df$chrom == 'X' & df$gene != "21_0",] %>%
#   group_by(ind) %>%
#   mutate(normalized = count / df[df$gene == "DMD" & df$ind == ind,]$count)
# #View(xchr_normalized)
# 
# ychr_normalized <- df[df$chrom == 'Y',] %>%
#   group_by(ind) %>%
#   mutate(normalized = count / df[df$gene =="chrY.background" & df$ind == ind,]$count)


# New version species specific ac's
xchr_normalized <- df[df$chrom == 'X',] %>%
  group_by(ind) %>%
  mutate(normalized = count / df[df$gene == "DMD" & df$ind == ind,]$count)
#View(xchr_normalized)

ychr_normalized <- df[df$chrom == 'Y',] %>%
  group_by(ind) %>%
  mutate(normalized = count / df[df$gene =="chrY.background" & df$ind == ind,]$count)



#boxplot
pdf("x_all_box.pdf")
ggplot(xchr_normalized, aes(x=gene, y=normalized)) + 
  geom_boxplot() + 
  ggtitle("X chr: Distribution of ampliconic region copy number among 12 hominids") + 
  labs(y="median copy number (normalized to DMD)", x="ampliconic region id") +
  coord_flip()
dev.off()

#bar
pdf("x_all_bar.pdf")
ggplot(xchr_normalized, aes(x=gene, y=normalized, color=sex)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~ ind+species) + 
  ggtitle("X chr: Ampliconic region copy number among 12 hominids") + 
  labs(y="median copy number (normalized to DMD)", x="ampliconic region id") +
  theme(title= element_text(size=12), axis.text.x = element_text(angle=90, hjust=1, size=3)) 
dev.off()

#inds
for (i in unique(xchr_normalized$gene)) {
  pdf(paste("x_gene_", i, ".pdf", sep=""))
  print(
    ggplot(xchr_normalized[xchr_normalized$gene==i,], aes(reorder(ind, normalized), y=normalized, fill=species)) +
      theme_bw() +
      #scale_fill_manual(values=c("#F47570", paste("#", 0.7*0xF4, 0.7*0x75, 0.7*0x70, sep=""), "#FF2222", "#F7F700", "#FFAA00", "#FF2222")) +
      #scale_color_manual(values=c("#FFFFFF", "#000000"))+ #f m
      ggtitle(paste("X chr: ampliconic region:", i)) +
      labs(y="copy number (normalized to DMD)", x="individual") +
      geom_bar(stat="identity") +
      geom_text(aes(label=sex, ), vjust=-0.12)
  )
  dev.off()
  print(i)
}

################################### Y

#boxplot
pdf("y_all_box.pdf")
ggplot(ychr_normalized, aes(x=gene, y=normalized)) + 
#  theme_bw() +
  geom_boxplot() + 
  ggtitle("Y chr: Distribution of ampliconic region copy number among 12 hominids") + 
  labs(y="median copy number (normalized to chrY.background)", x="ampliconic region id") +
  coord_flip()
dev.off()

#bar
pdf("y_all_bar.pdf")
ggplot(ychr_normalized, aes(x=gene, y=normalized, color=sex)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~ ind+species) + 
  #coord_flip() + 
  ggtitle("Y chr: Ampliconic region copy number among 12 hominids") + 
  labs(y="median copy number (normalized to chrY.background)", x="ampliconic region id") +
  theme(title= element_text(size=12), axis.text.x = element_text(angle=90, hjust=1, size=8)) 
dev.off()

#inds
for (i in unique(ychr_normalized$gene)) {
  pdf(paste("y_gene_", i, ".pdf", sep=""))
  print(
    ggplot(ychr_normalized[ychr_normalized$gene==i,], aes(reorder(ind, normalized), y=normalized, fill=species)) +
      theme_bw() +
      #scale_fill_manual(values=c("#F47570", paste("#", 0.7*0xF4, 0.7*0x75, 0.7*0x70, sep=""), "#FF2222", "#F7F700", "#FFAA00", "#FF2222")) +
      #scale_color_manual(values=c("#FFFFFF", "#000000"))+ #f m
      ggtitle(paste("Y chr: ampliconic region:", i)) +
      labs(y="copy number (normalized to Y chr.background)", x="individual") +
      geom_bar(stat="identity") +
      geom_text(aes(label=sex, ), vjust=-0.12)
  )
  dev.off()
  print(i)
}



