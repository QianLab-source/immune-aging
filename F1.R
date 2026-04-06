
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


##### F1 A and F1 B #####
Aging_immune_parameters = read.csv('github data\\Figure1A data.csv', check.names = F)
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

pheatmap(t(Aging_immune_parameters), color=colorRampPalette(c('cyan','black','yellow'))(100),
         clustering_distance_cols = "euclidean", 
         clustering_distance_rows = "euclidean",
         fontsize_col=0.5, fontsize_row=0.5,
         cluster_col=FALSE,
         breaks=seq(-3,3,length.out=99),annotation_row = cls.df,
         clustering_method = "complete",show_rownames = T,show_colnames=F)

list_cluster=list()
for(i in 1:6){
  df=Aging_immune_parameters[,which(colnames(Aging_immune_parameters) %in% colnames(Aging_immune_parameters)[which(cls.df$. %in% i)])] %>% 
    as.data.frame() %>% 
    mutate(age=sort(unique(agingcell$age)))
  list_cluster[[i]]=df
}

list_plot=list()
m=c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')
color1=c("#87cefa","#fa8072","#99CC33","#f4a460","#CCCCCC","#99CCCC")
color2=c("#4169e1","#CC3333","#228b22","#FF7F00","#4a4a4a","#20b2aa")
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
    geom_smooth(aes(group = indicator), color = color1[which(c(3,4,2,5,6,1)==i)],method = "loess", se = FALSE,size=0.5) + #size=0.7
    stat_smooth(data = df_mean, mapping = aes(x = age, y = mean_value), 
                method = "loess", size = 1, color = color2[which(c(3,4,2,5,6,1)==i)], se = FALSE) + # size=2
    labs(title=title,
         x="Age (years)",
         y="Immune parameters (z score)")+ 
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

p=wrap_plots(list_plot, ncol = 3)
p
ggsave('trajectory.pdf', width = 11.5, height = 8)

##### F1 C and F1 D#####
data = read.csv('github data\\Figure1C and D data.csv', check.names = F)

F1C <- ggplot(data, aes(x = Age , y = data[,2])) +  
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
F1C
F1D <- ggplot(data, aes(x = Age , y = data[,3])) +  
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
F1D

##### F1 F #####  
data = read.csv('github data\\Figure1F data.csv', check.names = F)
b=data$variable[data$group=='Peak cell']
b=b[order(data$`effect size abs`,decreasing = F)]
b=gsub('cells','',b)
data$variable=gsub('cells','',data$variable)
data$variable=factor(data$variable,levels = b)

p1=ggplot()+
  geom_col(
    data = filter(subset(data,data$group=='Peak cell'), dir == "Positive"),
    mapping = aes(y = variable, x = -1* `effect size abs`,fill = `-log10 (P)` ),
    width = 0.7
  ) +
  scale_fill_gradient(
    name = "-log10 (P)\nPositive",
    limits=c(10,45),
    low  = "mistyrose",
    high = "darkred",
    na.value = "grey50"
  ) +
  ggnewscale::new_scale_fill() +
  geom_col(
    data = filter(subset(data,data$group=='Peak cell'), dir == "Negative"),
    mapping = aes(y = variable,   x = -1* `effect size abs`,fill = `-log10 (P)` ),
    width = 0.7
  ) +
  scale_fill_gradient(
    name = "-log10 (P)\nNegative",
    limits=c(10,45),
    low  = "lightblue",
    high = "navy",
    na.value = "grey50"
  ) +
  theme_bw() +
  labs(title = "Peak cell",x='',y = "",fill = "" ) +
  scale_x_continuous(limits = c(-0.12,0),
                     breaks = c(-0.1,-0.05,0),
                     labels = c('0.10','0.05','0'),
                     expand = expansion(mult = c(0.06, 0.06)) ) +  
  theme(panel.grid.major.y = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1,color='black'),
    legend.position = 'none',
    title = element_text(hjust=0.5),
    axis.text.y = element_text(color = 'black',size=14),
    axis.text.x = element_text(color='black',size=12)
  )
p1

p2=ggplot()+
  geom_col(
    data = filter(subset(data,data$group=='Aging related cell'), dir == "Positive"),
    mapping = aes(y = variable, x =  `effect size abs`,fill = `-log10 (P)` ),
    width = 0.7
  ) +
  scale_fill_gradient(
    name = "-log10 (P)\nPositive",
    limits=c(10,45), 
    low  = "mistyrose",
    high = "darkred",
    na.value = "darkred"
  ) +
  ggnewscale::new_scale_fill() +
  geom_col(
    data = filter(subset(data,data$group=='Aging related cell'), dir == "Negative"),
    mapping = aes(y = variable,   x =`effect size abs`,fill = `-log10 (P)` ),
    width = 0.7
  ) +
  scale_fill_gradient(
    name = "-log10 (P)\nNegative",
    limits=c(10,45),
    low  = "lightblue",
    high = "navy",
    na.value = "navy"
  ) +
  theme_bw() +
  labs(title = "Aging related cell",x='',y = "",fill = "" ) +
  scale_x_continuous(limits = c(0,0.5),
                     breaks = c(0,0.2,0.4),
                     labels = c('0','0.2','0.4'),
                     expand = expansion(mult = c(0.06, 0.06)) ) +  
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1,color='black'),
        legend.title = element_text(size=10,hjust = 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size=10),
        title = element_text(hjust=1),
        axis.line.y.left  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = 'black',size=12)
  )
p2

p1+p2+plot_layout(guides = 'collect')+ plot_layout(widths = c(2.1, 2))

##### F1 G #####
data = read.csv('github data\\Figure1G data.csv', check.names = F)

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
# ggsave('github data\\F1G.pdf', width = 4.5, height = 7)

