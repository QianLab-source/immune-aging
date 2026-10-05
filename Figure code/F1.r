library(devtools)
library(gground)
library(pheatmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(openxlsx)
library(ggnewscale)
library(ggprism)

##### F1 B and F1 C #####
Aging_immune_parameters = read.csv('Figure1B data.csv', check.names = F)
dat <- t(Aging_immune_parameters)

list1=pheatmap(t(Aging_immune_parameters), 
               color=colorRampPalette(c('cyan','black','yellow'))(100),
               clustering_distance_cols = "euclidean", 
               clustering_distance_rows = "euclidean",
               fontsize_col=0.4, fontsize_row=0.4,
               cluster_col=FALSE,
               breaks=seq(-3,3,length.out=99),
               clustering_method = "complete",
               show_rownames = T,show_colnames=F)
cls.df=cutree(list1$tree_row,k=6) %>% data.frame()
table(cls.df)

png('F1B.png', bg = 'transparent', width = 8, height = 10, units = "in", res = 300)
pheatmap(t(Aging_immune_parameters), color=colorRampPalette(c('cyan','black','yellow'))(100),
         clustering_distance_cols = "euclidean", 
         clustering_distance_rows = "euclidean",
         fontsize_col=0.5, fontsize_row=0.5,
         cluster_col=FALSE,
         legend = FALSE, 
         breaks=seq(-3,3,length.out=99),annotation_row = cls.df,
         clustering_method = "complete",show_rownames = T,show_colnames=F)
dev.off()

list_cluster=list()
for(i in 1:6){
  df=Aging_immune_parameters[,which(colnames(Aging_immune_parameters) %in% colnames(Aging_immune_parameters)[which(cls.df$. %in% i)])] %>% 
    as.data.frame() %>% 
    mutate(age=20:60)
  list_cluster[[i]]=df
}

list_plot=list()
m=c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')
color1=c("#a8c9de","#d1827b","#9ab26f","#ddb695","#cacac9","#80b0b0")
color2=c("#39588f","#a6352f","#2c7839","#d07229","#4c4a4a","#2d928a")
for (i in c(3,4,2,5,6,1)) {
  clusterx=list_cluster[[i]]
  df_long <- clusterx %>%
    tidyr::gather(key = "Indicator", value = "value", -age)
  df_mean <- df_long %>% group_by(age) %>%
    summarise(mean_value = mean(value, na.rm = TRUE))
  df_long <- pivot_longer(clusterx, cols = -age, names_to = "indicator", values_to = "value")
  
  title=m[which(c(3,4,2,5,6,1)==i)]
  p <- ggplot(df_long, aes(x = age, y = value, color=indicator)) +
    guides(color = FALSE) + 
    geom_smooth(aes(group = indicator), color = color1[which(c(3,4,2,5,6,1)==i)],method = "loess", se = FALSE,size=0.7) + #size=0.7
    stat_smooth(data = df_mean, mapping = aes(x = age, y = mean_value), 
                method = "loess", size = 2, color = color2[which(c(3,4,2,5,6,1)==i)], se = FALSE) + # size=2
    labs(title=title,
         x="Age (years)",
         y="Immunophenotypes (z score)")+ 
    theme(plot.title = element_text(size=20,hjust = 0.5,color = "black"),
          axis.title.x = element_text(size = 16,color = "black"),
          axis.title.y = element_text(size = 16,color = "black"),
          axis.text.x = element_text(size = 14,color = "black"),
          axis.text.y = element_text(size = 14,color = "black"),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          panel.grid.major = element_blank())
  list_plot[[m[i]]]=p
}

p=wrap_plots(list_plot, ncol = 2)
p


##### F1 D and F1 E#####
data = read.csv('Figure1D and E data.csv', check.names = F)

F1D <- ggplot(data, aes(x = Age , y = data[,2])) +  
  geom_line(linetype = 1, linewidth = 1,color='black') + 
  geom_point(size = 3,color='black')+ 
  geom_vline(xintercept = 37, linetype = "solid", color = "#CD2626", size = 1.5) + 
  labs(x = 'Age (years)', y = 'No. of variable immunophenotypes')+
  theme_minimal(base_size = 8) +
  theme(    panel.grid = element_blank(), 
            panel.border = element_rect(fill = NA,size = 1,color = '#1C1C1C'), 
            axis.ticks = element_line(size = 0.5),
            axis.ticks.length = unit(1.5, "mm"),
            axis.text = element_text(size = 16,color = 'black'),
            legend.text = element_text(size = 16,color = 'black'),
            axis.title = element_text(size = 18,color = 'black')
  )
F1D
F1E <- ggplot(data, aes(x = Age , y = data[,3])) +  
  geom_line(linetype = 1, linewidth = 1,color='black') + 
  geom_point(size = 3,color='black')+ 
  geom_vline(xintercept = 38, linetype = "solid", color = "#CD2626", size = 1.5) + 
  labs(x = 'Age (years)', y = 'No. of significant immunophenotypes')+
  theme_minimal(base_size = 8) +
  theme(    panel.grid = element_blank(), 
            panel.border = element_rect(fill = NA,size = 1,color = '#1C1C1C'), 
            axis.ticks = element_line(size = 0.5),
            axis.ticks.length = unit(1.5, "mm"),
            axis.text = element_text(size = 16,color = 'black'),
            legend.text = element_text(size = 16,color = 'black'),
            axis.title = element_text(size = 18,color = 'black')
  )
F1E

##### F1 G #####
data = read.csv('Figure1G data.csv', check.names = F)

use_pathway <- data %>%
  group_by(ONTOLOGY) %>%
  slice_min(order_by = p.adjust, n = 8, with_ties = FALSE) %>% 
  group_by(p.adjust) %>%  
  ungroup() %>%
  mutate(ONTOLOGY = factor(ONTOLOGY,
                           levels = rev(c('BP', 'CC', 'MF')))) %>%
  arrange(ONTOLOGY, -p.adjust) %>%
  mutate(Description = factor(Description, levels = Description)) %>%
  tibble::rowid_to_column('index')

width <- 0.5
xaxis_max <- max(-log10(use_pathway$p.adjust)) + 1
rect.data <- group_by(use_pathway, ONTOLOGY) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  mutate(
    xmin = -2.5 * width,
    xmax = -0.5 * width,
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0) + 0.6,
    ymax = ymax + 0.4
  )

pal=c('#E0C09C','#a1a9d0',"#CC9999")
use_pathway %>%
  ggplot(aes(-log10(p.adjust), y = index, fill = ONTOLOGY)) +
  geom_round_col(
    aes(y = Description), width = 0.7, alpha = 0.8
  ) +
  geom_text(
    aes(x = 0.07, label = Description),
    hjust = 0, size = 4
  ) +
  geom_round_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = ONTOLOGY),
    data = rect.data,
    radius = unit(2, 'mm'),
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = ONTOLOGY),
    data = rect.data,
    inherit.aes = FALSE
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = xaxis_max, yend = 0),
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  labs(y = NULL,x='-log10 (P)') +
  scale_fill_manual(name = 'Category', values = pal) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(
    breaks = seq(0, xaxis_max, 2), 
    expand = expansion(c(0, 0))
  ) +
  theme_prism() +
  theme(
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size=10,colour = 'black'),
    axis.title.x = element_text(size=12,colour = 'black',margin = margin(t = 10)),
    legend.position = 'none',
    plot.margin = unit(c(0.2,1,0.2,1),'cm')
  )


