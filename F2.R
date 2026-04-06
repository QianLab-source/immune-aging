setwd('E:\\Fudan Lab\\Lab-Feng-Zhang Alin\\immune aging\\投稿')

library(ggplot2)
library(openxlsx)
library(ggrepel)
library(dplyr)
library(reshape2)
library(rstatix)
library(ggpubr)
library(ggbreak)
library(gground)
library(ggprism)

install.packages('ggprism')
if (!require("devtools", quietly = TRUE))
  install.packages("devtools")

devtools::install_github("dxsbiocc/gground")

##### F2A #####
data = read.csv('github data\\Figure2A data.csv',check.names = F)

immage_dot=function(s,x,y,ytitle,pcc,mae){
  ggplot(s,aes(x=x,y=y))+
    geom_point(size=3,alpha=0.5,color='black')+
    geom_smooth(color="blue",method="lm",se=FALSE)+
    labs(x="Chronological age(years)",y=ytitle,size=20)+
    xlim(20,60)+
    ylim(20,60)+
    theme_bw(base_size = 20) +
    theme(axis.text = element_text(color = 'black'),
          panel.grid.major=element_line(colour=NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank())+
    geom_text(aes(label=paste0('PCC = ',pcc),x=55,y=23))+
    geom_text(aes(label=paste0('MAE = ',mae),x=55,y=20))  
  
} 

pcc=round(cor(data$ImmuneAge,data$Age),2)
mae=round(mean(abs(data$ImmuneAge-data$Age)),2)
p1=immage_dot(s=data,x=data$Age,y=data$ImmuneAge, pcc=pcc,mae=mae,ytitle='ImmuneAge (years)')
p1

pcc=round(cor(data$`Adaptive immune age`,data$Age),2)
mae=round(mean(abs(data$`Adaptive immune age`-data$Age)),2)
p2=immage_dot(s=data,x=data$Age,y=data$`Adaptive immune age`,pcc=pcc,mae=mae, ytitle='AdaptiveAge (years)')
p2

pcc=round(cor(data$`Innate immune age`,data$Age),2)
mae=round(mean(abs(data$`Innate immune age`-data$Age)),2)
p3=immage_dot(s=data,x=data$Age,y=data$`Innate immune age`,pcc=pcc,mae=mae,ytitle='InnateAge (years)')
p3

##### F2C #####
data = read.csv('github data\\Figure2C data.csv',check.names = F)
data = data %>%
  mutate(sig = case_when(
    qval < 0.01 & rho <= -0.2 ~ 'Down',
    qval < 0.01 & rho >= 0.2 ~ 'Up',
    TRUE ~ 'None'
  ))
up = subset(data, sig == 'Up') %>%
  arrange(qval) %>%
  head(5)
down = subset(data, sig == 'Down') %>%
  arrange(qval) %>%
  head(3)
p <- ggplot(data, aes(x = rho, y = -log10(qval), color = sig)) +
  geom_point(size = 3) +
  scale_colour_manual(values  = c('#D1392B', '#8AB1D2', 'gray'), limits = c('Up', 'Down', 'None')) +
  geom_vline(xintercept = c(-0.2, 0.2), color = 'gray', size = 0.5) +
  geom_hline(yintercept = -log(0.01, 1), color = 'gray', size = 0.5) +
  xlim(-0.5, 0.5) + ylim(0, 50) +
  labs(x = 'Correlation coefficient', y = '-Log10 (q-value)', color = '', title = '')+
  theme_classic()+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.13,0.87),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
    axis.text.x   = element_text(color = 'black', size = 15, angle = 0),
    axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
    axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
    axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
    panel.border = element_rect(color = "black", fill = NA, size = 1))+
  geom_text_repel(data = rbind(up, down), aes(x = rho, y = -log10(qval), label = variable),
                  size = 3,box.padding = unit(0.5, 'lines'), segment.color = 'black', show.legend = FALSE)
p
ggsave('F2C.pdf',width = 4.2,height = 4.2)

##### F2D #####
data = read.csv('github data\\Figure2D data.csv',check.names = F)
data = as.data.frame(na.omit(data[,c(1,2,4,5,7)]))
colnames(data) = c('ID','ImmuneAgeAccel_rho','ImmuneAgeAccel_qval','Age_rho','Age_qval')
data$group = ifelse(data$ImmuneAgeAccel_qval<0.001 & data$Age_qval<0.001,'q<0.001',
ifelse(data$ImmuneAgeAccel_qval<0.01 & data$Age_qval<0.01,'q<0.01',ifelse(data$ImmuneAgeAccel_qval<0.05 & data$Age_qval<0.05,'q<0.05','NS')))

data$group = factor(data$group, levels = c('q<0.001','q<0.01','q<0.05','NS'))
p <- ggplot(data, aes(x = data[,'Age_rho'], y = data[,'ImmuneAgeAccel_rho'])) +
      geom_point(size = 2, aes(color = group)) + 
      scale_color_manual(values = c('#D1392B', '#8AB1D2', '#06539B', 'gray')) +
      labs(x = 'Age association', y = 'ImmuneAgeAccel association', color = '', title = '')+
      stat_cor(method = 'spearman', size = 4) +
      geom_smooth(method = 'lm', se = FALSE, color = 'black') +
      theme_classic()+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.15,0.7),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
    axis.text.x   = element_text(color = 'black', size = 15, angle = 0),
    axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
    axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
    axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
    panel.border = element_rect(color = "black", fill = NA, size = 1)) 
p
ggsave('github data\\F2D.pdf',width = 4.2,height = 4.2)

##### F2E #####
data = read.csv('github data\\Figure2E data.csv',check.names = F)
data[,-ncol(data)]<-lapply(data[,-ncol(data)], function(x) replace(100*as.numeric(x),is.na(x),median(x,na.rm = TRUE)))

ADplot = function(dat, step_increase, label_cell){
  dat <- dat[,c(label_cell,'group')] %>%
    arrange(match(group, c("Decelerated","Normal","Accelerated")))
  
  df <- melt(dat, id="group", variable.name="cell", value.name="value")
  mean <- aggregate(df$value, by=list(df$group, df$cell), FUN=mean)
  sd <- aggregate(df$value, by=list(df$group, df$cell), FUN=sd)
  len <- aggregate(df$value, by=list(df$group, df$cell), FUN=length)
  df_res <- data.frame(mean, sd=sd$x, len=len$x)
  colnames(df_res) = c("group", "cell", "Mean", "Sd", "Count")
  
  df <- df[df$group != "Normal",]
  df$group <- factor(df$group, levels=c("Decelerated","Accelerated"))
  df_p_val1 <- df %>%
    group_by(cell) %>%
    wilcox_test(formula = value ~ group) %>%
    add_significance(p.col = 'p', cutpoints = c(0,0.001,0.01,0.05,1), 
                     symbols = c('***','**','*','ns')) %>%
    add_xy_position(x='cell', step.increase = step_increase)
  df$cell <- factor(df$cell, levels = label_cell)
  
  is_mono <- grepl('mono', label_cell[1])
  
  p <- ggplot(df, aes(x=cell, y=value, fill=group)) +
    geom_boxplot(width=0.5, show.legend = !is_mono) +
    scale_fill_manual(values = c("#70ABC6","#DE6B54")) +
    stat_pvalue_manual(df_p_val1, label = '{p.signif}', tip.length = 0, size=6) +
    labs(x='', y='Cell proportion (%)') +
    guides(fill=guide_legend(title = '')) +
    theme_classic() +
    theme(
      axis.text = element_text(color = 'black'),
      plot.margin = if(is_mono) unit(c(3, 0, 0, 5), "cm") 
                    else unit(c(1, 0.5, 0.5, 0.5), "cm"),
      legend.position = c(0.8, 0.9),
      legend.text = element_text(size = if(is_mono) 14 else 13, color = 'black'),
      axis.text.x = element_text(color = 'black', size = 14, angle = 20, hjust = 1),
      axis.text.y = element_text(color = 'black', size = 14),
      axis.title.x = element_text(color = 'black', size = 16),
      axis.title.y = element_text(color = 'black', size = 16, angle = 90)
    ) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    scale_x_discrete(labels = label_cell)
  
  if(is_mono) {
    p <- p +
      theme(axis.line.y.right = element_blank(),
            axis.ticks.y.right = element_blank(),
            axis.text.y.right = element_blank(),
            axis.title.y.right = element_blank()) +
      scale_y_break(c(25, 60), space = 0.5, scales = 1)
  }
  
  return(p)
}

p1 = ADplot(dat = data, step_increase = 0.03, label_cell = c("Classical mono","Intermediate mono","Non-Classical mono"))
p1

p2 = ADplot(dat = data, step_increase = 0.04, label_cell = c("Naive CD8+ T","Naive CD4+ T","Naive Treg"))
p2

p3 = ADplot(dat = data, step_increase = 0.05, label_cell = c("CD28- CD3+ T","CD85j+ CD3+ T","CD57+ CD3+ T"))
p3

##### F2F #####
data = read.csv('github data\\Figure2F data.csv',check.names = F)
a = data$SYMBOL[data$qvalue<0.05]
length(a)
data = data %>%
  mutate(sig = case_when(
    `qvalue` < 0.05 & `Correlation coefficient` < 0 ~ 'Down',
    `qvalue` < 0.05 & `Correlation coefficient` > 0 ~ 'Up',
    TRUE ~ 'None'
  ))

up <- subset(data, sig == 'Up')
up <- up[order(up$`qvalue`), ][1:5, ]
down <- subset(data, sig == 'Down')
down <- down[order(down$`qvalue`), ][1:10, ]

p=ggplot(data, aes(x = `Correlation coefficient`, y = -log10(`qvalue`), color = sig)) +
  geom_point( size = 3) +
  scale_colour_manual(values  = c('#CC3A34', '#4A74AA', 'gray'), limits = c('Up', 'Down', 'None')) +
  geom_vline(xintercept = c(-0.1, 0.1), color = 'gray', size = 0.5) +
  geom_hline(yintercept = -log(0.05, 1), color = 'gray', size = 0.5) +
  xlim(-0.3, 0.3) + 
  # ylim(0, 50) +
  labs(x = 'Correlation coefficient', y = '-Log10 (q-value)', color = '', title = '')+
  theme_bw()+
  theme_classic()+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.position = c(0.85, 0.85),
    plot.title  = element_text(color = 'black', size = 15,hjust=0.5),
    axis.text.x   = element_text(color = 'black', size = 15, angle = 0),
    axis.text.y   = element_text(color = 'black', size = 15, angle = 0),
    axis.title.x  = element_text(color = 'black', size = 15, angle = 0),
    axis.title.y  = element_text(color = 'black', size = 15, angle = 90),
    panel.border = element_rect(color = "black", fill = NA, size = 1))+
  geom_text_repel(data = rbind(up, down), aes(x = `Correlation coefficient`, y = -log10(`qvalue`), label = SYMBOL),
                  size = 3,box.padding = unit(0.5, 'lines'), point.padding = unit(0.6, "lines"),
                  force = 2,segment.color = 'black', show.legend = FALSE)
p

##### F2G #####
data = read.csv('github data\\Figure2G data.csv',check.names = F)

use_pathway <- data %>%
  subset(., p.adjust<0.05) %>% 
  group_by(ONTOLOGY) %>%
  slice_min(order_by = p.adjust, n = 8, with_ties = FALSE) %>%
  slice_max(order_by = Count, n = 8, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(ONTOLOGY = factor(ONTOLOGY,
                           levels = rev(c('BP', 'CC', 'MF')))) %>% #, 'KEGG'
  arrange(ONTOLOGY, -p.adjust) %>%
  mutate(Description = factor(Description, levels = Description)) %>%
  tibble::rowid_to_column('index')

width <- 0.4
xaxis_max <- max(-log10(use_pathway$p.adjust)) + 1
rect.data <- group_by(use_pathway, ONTOLOGY) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  mutate(
    xmin = -1 * width,
    xmax = -0.3 * width,
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
    aes(x = 0.05, label = Description),
    hjust = 0, size = 4.1
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
    axis.title.x = element_text(size=12,colour = 'black'), 
    legend.position = 'none',
    plot.margin = unit(c(0.2,0.5,0.2,0.5),'cm')
  )
ggsave('github data\\F2G.pdf',width = 5, height = 8)

##### F2H #####
data = read.csv('github data\\Figure2H data.csv',check.names = F)
data2$class=factor(data2$class,levels = c('Cardiovascular risk factors','Cognitive decline'))

ggplot(data2,aes(x = beta,y = reorder(variable,beta),xmin = min, xmax = max)) +
  geom_point(aes(color = class),size = 4) +
  geom_errorbarh(aes(color=class),height = 0,show.legend = F,size=0.8) +
  geom_vline(xintercept = 0,linetype="dashed") +
  facet_grid(class~.,scales = 'free',space = 'free')+
  xlab("Coefficient")+ylab("")+
  ggtitle('')+
  theme_minimal()+
  theme_classic()+
  scale_color_manual(values = c("#CC6666","#6699CC"))+
  xlim(-0.5,0.8)+
  theme(plot.title = element_text(hjust = 0.5,color='black'),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.5), "cm"),
        strip.text = element_text(size=10),
        axis.text.y = element_text(size = 12,color='black'),
        axis.title.x = element_text(size = 14,color='black'),
        axis.text.x = element_text(size = 12,color='black'))+
  geom_text(aes(label = ifelse(pvalue < 0.001, "***",ifelse(pvalue < 0.01, "**",ifelse(pvalue < 0.05, "*","")))),
            x = ifelse(data2$beta>0, data2$max+0.005,data2$min-0.005),
            hjust = -0.3, vjust = 0.8,show.legend = F,size=6)

